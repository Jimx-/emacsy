;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Emacsy.
;;;
;;; Emacsy is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.

(define-module (emacsy buffer)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (emacsy mode))

;;; Commentary:

;; @node Buffer
;; @section Buffer

;; @quotation
;; And when you gaze long into an abyss the abyss also gazes into you.
;; @author Beyond Good and Evil, Friedrich Nietzsche
;; @end quotation

;; A buffer in Emacs represents text, including its mode, local
;; variables, etc.  A Emacsy buffer is not necessarily text.  It can be
;; extended to hold whatever the host application is interested in.
;; Emacs' concepts of buffer, window, and mode are directly analogous to
;; the model, view, and controller respectively---the MVC pattern.

;;; Code:

;; @defmac with-buffer @dots{}
;; A convenience macro to work with a given buffer.
;; @end defmac
;;.
(define-syntax-public with-buffer
  (syntax-rules ()
    ((with-buffer buffer e ...)
     (let ((old-buffer (current-buffer))
           (result *unspecified*))
       (in-out-guard
        (lambda () (set-buffer! buffer))
        (lambda () e ...)
        (lambda () (set-buffer! old-buffer)))))))

;; @defmac save-excursion @dots{}
;; A convenience macro to do some work
;; @end defmac
;;.
(define-syntax-public save-excursion
  (syntax-rules ()
    ((save-excursion body ...)
     (let ((old-buffer (current-buffer))
           (old-point (point))
           (old-mark (mark)))
       (in-out-guard
        (lambda _ #t)
        (lambda _ body ...)
        (lambda _
          (set-buffer! old-buffer)
          (set-mark old-mark)
          (goto-char old-point)))))))

;;.
(define-class-public <buffer> ()
  (name #:init-keyword #:name)
  (keymap #:accessor local-keymap #:init-keyword #:keymap #:init-form (make-keymap))
  (locals #:accessor local-variables #:init-form '())
  (buffer-modified? #:accessor buffer-modified? #:init-value #f)
  (buffer-modified-tick #:accessor buffer-modified-tick #:init-value 0)
  (buffer-enter-hook #:accessor buffer-enter-hook #:init-form (make-hook 0))
  (buffer-exit-hook #:accessor buffer-exit-hook #:init-form (make-hook 0))
  (buffer-modes #:accessor buffer-modes #:init-form '() #:init-keyword #:buffer-modes))
(export local-keymap local-variables buffer-enter-hook buffer-exit-hook before-buffer-change-hook after-buffer-change-hook after-change-hook before-change-hook buffer-modified-tick buffer-modes)

;;.
(define-variable before-buffer-change-hook (make-hook 1) "This hook is called prior to the buffer being changed with one argument, the buffer.")

;;.
(define-variable after-buffer-change-hook (make-hook 1) "This hook is called after to the buffer has changed with one argument, the buffer.")

;;; The buffer module also keeps track of the live buffers and the current
;;; one.

;;.
(define-public buffer-stack (make <mru-stack>))

;;.
(define-public last-buffer #f)

;;.
(define-public aux-buffer #f)

;; Buffer's have a name, and there is always a current buffer or it's
;; false.  Note that methods do not work as easily with optional
;; arguments.  It seems best to define each method with a different
;; number of arguments as shown below.
(define-method-public (buffer-name)
  (buffer-name (current-buffer)))

;;.
(define-method-public (buffer-name (buffer <buffer>))
  (slot-ref buffer 'name))

;;.
(define-method-public (set-buffer-name! name)
  (set-buffer-name! name (current-buffer)))

;;.
(define-method-public (set-buffer-name! name (buffer <buffer>))
  (slot-set! buffer 'name name))

;;.
(define-method-public (buffer-modified?)
  (buffer-modified? (current-buffer)))

;;.
(define-method-public (buffer-modified-tick)
  (buffer-modified-tick (current-buffer)))

;;.
(define-method (write (obj <buffer>) port)
  (write (string-concatenate (list "#<buffer '" (buffer-name obj) "'>")) port))

;; @c @node
;; @subsection Emacs Compatibility

;;.
(define-public (current-local-map)
  (local-keymap (current-buffer)))

;;.
(define-public (use-local-map keymap)
  (set! (local-keymap (current-buffer)) keymap))

;;.
(define-public (buffer-list)
  (mru-list buffer-stack))

;;.
(define-public (current-buffer)
  ;; Perhaps instead of returning #f for no buffer there should be an
  ;; immutable void-buffer class.
  (or aux-buffer
      (mru-ref buffer-stack)))

;;.
(define-public (add-buffer! buffer)
  (mru-add! buffer-stack buffer))

;;.
(define-public (remove-buffer! buffer)
  (mru-remove! buffer-stack buffer))

;;.
(define-interactive (next-buffer #:optional (incr 1))
  (mru-next! buffer-stack incr)
  (switch-to-buffer (mru-ref buffer-stack)))

;;.
(define-interactive (prev-buffer #:optional (incr 1))
  (next-buffer (- incr)))

;;.
(define-public (set-buffer! buffer)
  ;;(emacsy-log-debug "set-buffer! to ~a" buffer)
  (if (mru-set! buffer-stack buffer)
      (set! aux-buffer #f)
      (set! aux-buffer buffer)))

;; This is scary, we will override it when we have <text-buffer>.
;;.
(define-interactive (kill-buffer #:optional (buffer (current-buffer)))
  (remove-buffer! buffer))

;;.
(define-interactive (other-buffer #:optional (count 1))
  (next-buffer count))

;;; This is our primitive procedure for switching buffers.  It does not
;;; handle any user interaction.
(define (primitive-switch-to-buffer buffer)
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer))
  (run-hook (buffer-exit-hook (current-buffer)))
  (set! last-buffer (current-buffer))
  (if (mru-contains? buffer-stack buffer)
      (begin
        (emacsy-log-debug "Recall buffer ~a" buffer)
        (mru-recall! buffer-stack buffer)
        (set! aux-buffer #f))
      (begin
        (emacsy-log-debug "Set buffer to ~a" buffer)
        (set-buffer! buffer)))
  (emacsy-log-debug "Running enter hook for ~a" (current-buffer))
  (run-hook (buffer-enter-hook (current-buffer)))
  (current-buffer))

;;.
(define-public switch-to-buffer primitive-switch-to-buffer)

;;.
(define (local-var-ref symbol)
  (let ((result (assq symbol (local-variables (current-buffer)))))
    (if (pair? result)
     (cdr result)
     ;(variable-ref (make-undefined-variable))
     (throw 'no-such-local-variable symbol))))

;; If buffers were in their own modules I could dynamically add variables
;; to their namespace.  Interesting idea.

;;.
(define (local-var-set! symbol value)
  (slot-set! (current-buffer)
             'locals
             (assq-set! (local-variables (current-buffer)) symbol value)))

;;.
(define-public local-var
               (make-procedure-with-setter local-var-ref local-var-set!))

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

;;; Commentary:

;; @node Core
;; @section Core

;; Now we're going to put in place some core functionality that makes
;; Emacsy an Emacs-like library.

;;; Code:

(define-module (emacsy core)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 q)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:use-module (oop goops)
  #:use-module (rnrs io ports)
  #:use-module (debugging assert)
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11) ;; let-values

  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy event)
  #:use-module (emacsy mode)
  #:use-module (emacsy buffer)
  #:use-module (emacsy text)
  #:use-module (emacsy command)
  #:use-module (emacsy block)
  #:use-module (emacsy klecl)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:replace (switch-to-buffer
             kill-buffer)
  #:export (read-from-mouse)
  #:declarative? #f)

;;; <core:macro>=
(define-syntax-public track-mouse
  (syntax-rules ()
    ((track-mouse e ...)
     (in-out-guard  ;; This is different from dynamic-wind.
       (lambda () (set! emacsy-send-mouse-movement-events? #t))
       (lambda () e ...)
       (lambda () (set! emacsy-send-mouse-movement-events? #f))))))


;; We need a global keymap.
(define-public global-map (make-keymap))

;;.
(define-public special-event-map (make-keymap))

;;.
(define-public emacsy-quit-application? #f)

;;; @subsection Universal-argument
;;;(define-parameter universal-argument-queue (make-q) "This holds the current universal argument value.")
(define universal-argument-queue (make-q))

(define-public buffer-classes (list <text-buffer>))

;;.
(define-public messages
  (make <text-buffer> #:keymap (make-keymap) #:name "*Messages*"))

;; We want to be able to define variables that are not redefined if a
;; source file or module is reloaded, just like [[define-once]].
;;
;; @c subsection Mouse Movement
;;
;; Sometimes we may want to track the motion events generated by a mouse.
;; We don't do this all the time because it seems unnecessarily taxing.
(define-public emacsy-send-mouse-movement-events? #f)
;;; <core:procedure>=
(define-public (current-minor-mode-maps)
  (list))

;;.
(define-public (current-active-maps)
  `(,(current-local-map) ,@(map mode-map (buffer-modes (current-buffer))) ,global-map))

(set! default-klecl-maps current-active-maps)
;;.
(define-public (universal-argument-ref)
  (if (q-empty? universal-argument-queue)
      1
      (q-front universal-argument-queue)))

;;.
(define-public (universal-argument-pop!)
  (if (q-empty? universal-argument-queue)
      1
      (q-pop! universal-argument-queue)))

;;.
(define-public (universal-argument-push! arg)
  (q-push! universal-argument-queue arg))

;;.
(define-interactive (switch-to-buffer #:optional buffer) #t)

(define-interactive
  (switch-to-buffer
   #:optional
   (buffer
    (let-values (((to-string from-string) (object-tracker buffer-name)))
      (from-string (completing-read "Buffer: "
                                    (map to-string (buffer-list))))))
   (buffer-class-arg #f))
  (define (coerce buffer)
    (if (is-a? buffer <string>)
        (or (find (lambda (b) (string=? buffer (buffer-name b))) (buffer-list))
            buffer)
        buffer))
  (set! buffer (coerce buffer))
  (if (is-a? buffer <buffer>)
   ((@@ (emacsy buffer) primitive-switch-to-buffer) buffer)
   (if (is-a? buffer <string>)
    ;; Create a new buffer
    (let* ((buffer-class (or buffer-class-arg ;;; <core:Choose a buffer class.>=
                                              (if (= (length buffer-classes) 1)
                                                  ;; If we only have one buffer class, use that.
                                                  (car buffer-classes)
                                                  ;; Otherwise, let the user choose one.
                                                  (let*-values
                                                      (((to-string from-string) (object-tracker
                                                                                 (compose symbol->string class-name))))
                                                    (let ((a-buffer-class
                                                           (from-string (completing-read "Buffer Class: "
                                                                                         (map to-string buffer-classes)))))
                                                      (if (is-a? a-buffer-class <class>)
                                                          a-buffer-class
                                                          (begin
                                                            (message "No such class ~a." a-buffer-class)
                                                            (throw 'invalid-class))))))))
           (new-buffer ;;; %\todo{XXX Should define-interactive export the symbol? NO.}
                       ;;;
                       ;;; We will need to register classes that will be available to the user to
                       ;;; instantiate.
                       ;;;
                       ;;;
                       ;;; <core:Create a new buffer of a certain class.>=
                       (make buffer-class #:name buffer)))
          (add-buffer! new-buffer)
          new-buffer)
    (begin (message "Buffer or buffer-name expected.")
           #f))))

(define echo-area "")

;;.
(define-public (emacsy-echo-area)
  echo-area)

;;.
(define-public (current-message)
  echo-area)

;;.
(define (emacsy-message . args)
  (let ((string (apply format #f args)))
   (with-buffer messages
                (insert string)
                (insert "\n"))
   (set! echo-area string)
   (if emacsy-interactive?
       (wait)
       (display string))
   string))

;; There's probably a better way to do this.
(set! message emacsy-message)

;; When the minibuffer is entered, we want to clear the echo-area.
;; Because the echo-area is defined in core, it seems best to deal with
;; it in core rather than placing echo-area handling code in minibuffer.
(define-public (clear-echo-area)
  (set! echo-area ""))

;; These are most of the C API calls.
(define-public (emacsy-message-or-echo-area)
  (if emacsy-display-minibuffer?
      (buffer-string)
      echo-area))

;;.
(define-public (emacsy-minibuffer-point)
  (if emacsy-display-minibuffer?
      (point)
      -1))

;;.
(define-public (emacsy-run-hook hook . args)
  (catch #t
    (lambda ()
      (if debug-on-error?
          (call-with-error-handling
           (lambda ()
             (apply run-hook hook args)
             #t))
          (with-backtrace*
           (lambda ()
             (apply run-hook hook args)
             #t))))
    (lambda (key . args)
      (emacsy-log-error "Hook ~a threw error '~a with args ~a " hook key args)
;      (emacsy-log-error "Resetting hook ~a" hook)
;      (reset-hook! hook)
      #f)))

(define-public emacsy-terminate-hook (make-hook))

;;.
(define-public (emacsy-terminate)
  (run-hook emacsy-terminate-hook))

;;.
(define-public (emacsy-tick)
  (define (my-tick)
    (update-agenda))

  (if debug-on-error?
      (call-with-error-handling
       (lambda ()
         (my-tick)))
      (with-backtrace*
       (lambda ()
         (my-tick)))))

(define* (read-from-mouse #:optional (prompt #f))
  (define (my-read-event)
    (if (and (pair? this-command-event)
             (mouse-event? (car this-command-event)))
        (let ((event (car this-command-event)))
          (set! this-command-event (cdr this-command-event))
          event)
          ;; XXX Should this be read-key or read-event?
        (read-event prompt)))
  (let loop ((event (my-read-event)))
    (if (mouse-event? event)
        ;; Got an event.
        (position event)
        (let ((canceled? #f))
          ;; Put this event back in the queue.
          (emacsy-event event)
          (catch
            'quit-command
            (lambda () (primitive-command-tick))
            (lambda (key . args)
              (emacsy-log-debug "READ-FROM-MOUSE CANCELED\n")
              (set! canceled? #t)))
          (if canceled?
              (throw 'quit-command 'quit-read-from-mouse)
              (loop (my-read-event)))))))
;;; @subsection Command Loop
;;;
;;; If we ever run out of command loops due to errors, we start a new one.
;;;
;;;
;;; <core:procedure>=
(codefine (restart-command-loop)
  ;; Start another command-loop
          (while #t
            (emacsy-log-warning "NO COMMAND LOOPS; STARTING ANOTHER.")
            (command-loop)))

(codefine (non-interactive-command-loop)
  ;; Start another command-loop
          (emacsy-log-warning "STARTING NON-INTERACTIVE COMMAND LOOP.")
          (catch #t
            (lambda () (primitive-command-loop))
            (lambda args
              (format #t "stopping non-interactive command loop ~a" args)))
          (exit 0)
          ;;(quit-application)
          )

;(agenda-schedule restart-command-loop)

;;.
(define-public (emacsy-initialize interactive?)
  (when (and #f interactive?)
    (agenda-schedule restart-command-loop))
  (agenda-schedule (if interactive?
                       restart-command-loop
                       non-interactive-command-loop))
  (set! emacsy-interactive? interactive?))

;; There is one command that I consider fundamental for an Emacs-like
;; program.  Whenever I'm presented with a program that claims to be
;; Emacs-like, I try this out @verb{|M-: (+ 1 2)|}. If it doesn't work then
;; it may have Emacs-like key bindings, but it's not Emacs-like.  That
;; command is [[eval-expression]].  Let's write it.
(define-interactive (eval-expression #:optional epression) #t)

(define-interactive (eval-expression #:optional (expression (read-from-string (completing-read "Eval: " apropos-completion-function))))
  (let ((value (eval expression (interaction-environment))))
    (message "~a" value)
    value))

;; The second fundamental command is [[execute-extended-command]] invoked
;; with @verb{|M-x|}.
(define-interactive (execute-extended-command #:optional (n 1))
  ;(display "HERE!\n")
  (let ((str (completing-read "M-x " (completer global-cmdset))))
    (call-interactively (or (module-variable (current-module) (string->symbol str))
                            (module-ref (resolve-module '(zem core commands)) (string->symbol str))))))

;;.
(define-interactive (quit-application)
  (set! emacsy-quit-application? #t)
  (wait))

;; This [[universal-argument]] command is written using a different style
;; than is typical for interative Emacs commands.  Most Emacs commands
;; are written with their state, keymaps, and ancillary procedures as
;; public variables.  This style has a benefit of allowing one to
;; manipulate or extend some pieces; however, there are some benefits to
;; having everything encapsulated in this command procedure.  For
;; instance, if the minibuffer were written in this style, one could
;; invoke recursive minibuffers.
(define-interactive (universal-argument)
  "Universal argument is used as numerical input for many functions."
  (let ((count 0)
        (ua-keymap (make-keymap))
        (prompt "C-u ")
        ;(acceptable-chars (char-set-adjoin char-set:digit #\-))
        (done? #f))
    (define (add-to-prompt string)
      (set! prompt (string-concatenate (list prompt string " "))))
    (define (process-arg number)
      (add-to-prompt (number->string number))
      (set! count (+ (* count 10) number)))
    (define (my-undefined-command key-sequence events)
      (if (= count 0)
          (universal-argument-push! 4)
          (universal-argument-push! count))
      (set! done? #t)
      (set! echo-area "") ;; Clear the echo area.
      (for-each emacsy-event-unread (reverse events)))
    (define-key ua-keymap "1" (lambda () (process-arg 1)))
    (define-key ua-keymap "2" (lambda () (process-arg 2)))
    (define-key ua-keymap "3" (lambda () (process-arg 3)))
    (define-key ua-keymap "4" (lambda () (process-arg 4)))
    (define-key ua-keymap "5" (lambda () (process-arg 5)))
    (define-key ua-keymap "6" (lambda () (process-arg 6)))
    (define-key ua-keymap "7" (lambda () (process-arg 7)))
    (define-key ua-keymap "8" (lambda () (process-arg 8)))
    (define-key ua-keymap "9" (lambda () (process-arg 9)))
    (define-key ua-keymap "0" (lambda () (process-arg 0)))
    (define-key ua-keymap "-" (lambda ()
                                (set! count (- count))
                                (add-to-prompt "-")))
    (define-key ua-keymap "C-u" (lambda ()
                                  (when (= count 0)
                                    (set! count 4))
                                  (set! count (* count 4))
                                  (add-to-prompt "C-u")))
    (while (not done?)
      (primitive-command-tick prompt
                              #:keymaps (list ua-keymap)
                              #:undefined-command my-undefined-command))
    (when #f
      (let loop ((input (read-key prompt)))
          ;; We only admit numbers and a dash
          (if (and (is-a? input <key-event>)
                   (null? (modifier-keys input))
                   (char-set-contains? acceptable-chars (command-char input)))
              (begin
                (cond
                 ((char=? (command-char input) #\-)
                  (add-to-prompt "-")
                  (set! count (- count)))
                 (else
                  (process-arg (string->number (string (command-char input))))))
                (loop (read-key prompt)))
              (begin
                (if (= count 0)
                    (universal-argument-push! 4)
                    (universal-argument-push! count))
                (emacsy-event-unread input)))))))

;; We want to be able to load a scheme file.
;; @c todo {We should have aread-filename procedure.}
(define-interactive (load-file #:optional file-name) #t)

(define-interactive
  (load-file #:optional (filename (read-file-name "Filename: ")))
  (catch #t
    (lambda _
      (load filename)
      (message "Loaded ~a." filename)
      #t)
    (lambda (key . args)
      (let ((error-msg
             (call-with-output-string
               (lambda (args) (apply display-error #f args)))))
        (message "Failed to load ~a: ~a" filename error-msg)
        #f))))

;; @c subsection Essential key bindings
;; And here are the essential key bindings.

(define-key global-map "M-:"        'eval-expression)
(define-key global-map "M-x"        'execute-extended-command)
(define-key global-map "C-g"        'keyboard-quit)
(define-key global-map "C-x C-c"    'quit-application)
(define-key global-map "C-u"        'universal-argument)

(define-key special-event-map "C-g" 'keyboard-quit)

;; @subsubsection Keyboard Macro Keybindings

(define-key global-map "C-x (" 'kmacro-start-macro)
(define-key global-map "C-x )" 'kmacro-end-macro)
(define-key global-map "C-x e" 'kmacro-end-and-call-macro)

;; @subsubsection Buffer Manipulation Keybindings

(define-key global-map "C-o"   'other-buffer)
(define-key global-map "C-x k" 'kill-buffer)
(define-key global-map "C-x b" 'switch-to-buffer)
;;; <core:process>=
(add-buffer! messages)
;;; <core:process>=
(add-hook! (buffer-enter-hook minibuffer)
           (lambda () (clear-echo-area)))
;;; <core:process>=
(add-hook! no-blocking-continuations-hook restart-command-loop)

(define-public fundamental-map
  (let ((keymap (make-keymap)))
    (char-set-for-each
     (lambda (c)
       (let ((event (make <key-event>
                      #:command-char c)))
         (define-key keymap (list (event->kbd event))
           'self-insert-command)))
     (list->char-set
      '(#\space)
      (char-set-delete
       (char-set-intersection char-set:ascii char-set:printing)
       #\vtab #\page #\nul)))
    (for-each
     (match-lambda ((key command) (define-key keymap key command)))
     `(("C-SPC" set-mark-command)
       ("C-x h" mark-whole-buffer)
       ("C-k" kill-line)
       ("C-w" kill-region)
       ("C-y" yank)
       ("M-y" yank-pop)
       ("C-x C-x" exchange-point-and-mark)
       ("C-f" forward-char)
       ("M-f" forward-word)
       ("C-b" backward-char)
       ("M-b" backward-word)
       ("M-DEL" backward-kill-word)
       ("M-d" kill-word)
       ("C-d" delete-forward-char)
       ("C-a" move-beginning-of-line)
       ("C-e" move-end-of-line)
       ("M-<" beginning-of-buffer)
       ("M->" end-of-buffer)
       ("C-n" next-line)
       ("C-p" previous-line)
       ("DEL" delete-backward-char)
       ("RET" ,(lambda _ (insert #\newline)))
       ("C-k" kill-line)
       ("C-M-w" append-next-kill)))
    keymap))

(define-public fundamental-mode (make <mode> #:mode-name "fundamental" #:mode-map fundamental-map))

(define*-public (make-text-buffer #:optional (name "*scratch*"))
  (let ((buffer (make <text-buffer> #:name name #:buffer-modes `(,fundamental-mode) #:keymap global-map)))
    (add-buffer! buffer)
    buffer))

;; The *scratch* buffer.
;;.
(define-public scratch (make-text-buffer "*scratch*"))
(buffer:insert-string scratch
                      ";; This buffer is for text that is not saved, and for Scheme evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
")

;; Override kill-buffer; make sure the buffer list does not become empty.
;;.
(define-interactive (kill-buffer #:optional (buffer (current-buffer)))
  "Safe variant of kill-buffer."
  (run-hook (buffer-kill-hook (current-buffer)))
  (remove-buffer! buffer)
  (when (null? (buffer-list))
    (set! scratch (make-text-buffer))))

(define-interactive (new-buffer #:optional (name "*scratch*"))
  (let ((buffer (make-text-buffer name))) ;; FIXME: we need a hook for this
    (switch-to-buffer buffer)
    buffer))

;;.
(define-interactive (find-file #:optional file-name)
  #t)

(define-interactive (find-file #:optional (file-name (read-file-name "Find file: ")))
  (catch #t
    (lambda _
      (let* ((buffer (new-buffer (basename file-name))) ;; FIXME: de-duplication <2> |dir etc
             (file-name (expand-file-name file-name))
             (exists? (access? file-name R_OK))
             (text (if exists? (with-input-from-file (expand-file-name file-name) read-string) "")))
        (set! (buffer-file-name buffer) file-name)
        (insert text)
        (goto-char (point-min))
        (when (and=> (buffer-file-name (current-buffer)) string?)
          (set! (local-var 'default-directory) (canonize-file-name (dirname (buffer-file-name buffer)))))
        (set! (buffer-modified-tick buffer) -1)
        (set! (buffer-modified? buffer) (not exists?))))
    (lambda (key subr msg args . rest)
      (let ((error-msg
             (with-output-to-string
               (lambda _
                 (display-error #f (current-output-port) subr msg args rest)))))
        (message "Failed to load ~a: ~a" file-name error-msg)))))

(define-key global-map "C-x C-f" 'find-file)

;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-
;;; \section{Windows (Optional)}
;;;
;;; Emacsy aims to offer the minimal amount of intrusion to acquire big
;;; gains in program functionality.  Windows is an optional module for
;;; Emacsy.  If you want to offer windows that behave like Emacs windows,
;;; you can, but you aren't required to.
;;;
;;;
;;; <file:window.scm>=
;;; \subsection{Legal Stuff}
;;;
;;; <+ Copyright>=
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; <+ License>=
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
;;; <window:Module>=
(define-module (emacsy window)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  ;;; <window:Include Modules>=
    #:use-module (ice-9 match)
  #:export (<window>
            <internal-window>
            window?
            split-window
            window-buffer
            window-children
            orientation
            user-data
            window-list
            root-window
            current-window
            window-configuration-change-hook
            window-clone)
  #:export-syntax (  )
)
;;; \section{Classes}
;;;
;;; The window class contains a renderable window that is associated with
;;; a buffer.
;;;
;;;
;;; <window:Classes>=
(define-class <window> ()
  (window-parent #:accessor window-parent #:init-value #f)
  (user-data #:accessor user-data #:init-keyword #:user-data #:init-value #f)
  (window-buffer #:accessor window-buffer #:init-keyword #:window-buffer #:init-value #f)
  (window-dedicated? #:accessor window-dedicated? #:init-value #f))
;;; The internal window class contains other windows.
;;;
;;;
;;; <window:Classes>=
(define-class <internal-window> ()
  (window-parent #:accessor window-parent #:init-value #f)
  (user-data #:accessor user-data #:init-keyword #:user-data #:init-value #f)
  (window-children #:accessor window-children #:init-keyword #:window-children #:init-value '())
  (orientation #:accessor orientation #:init-keyword #:orientation #:init-value 'vertical) ; or 'horizontal
  (size #:accessor size #:init-keyword #:size #:init-value .5))
;;; <window:State>=
(define-public root-window (make <window>))
;;; <window:State>=
(define-variable window-configuration-change-hook (make-hook 1) "This hook is called when a window is split.")
;;; <window:State>=
(define current-window #f)
;;;  \section{Procedures}
;;;
;;;
;;; <window:Procedures>=
(define-method (initialize (obj <internal-window>) initargs)
  (next-method)
  (for-each (lambda (window)
    (set! (window-parent window) obj)) (window-children obj)))

(define (window? o)
  (or (is-a? o <window>) (is-a? o <internal-window>)))
;;; <window:Procedures>=
(define (window-live? o)
  (is-a? o <window>))
;;; <window:Procedures>=
(define (frame-root-window)
  root-window)
;;; Emacs uses the edges of windows \verb|(left top right bottom)|, but
;;; I'm more comfortable using bounded coordinate systems
;;; \verb|(left bottom width height)|.  So let's write some converters.
;;;
;;;
;;; <window:Procedures>=
(define (edges->bcoords edges)
  (match edges
   ((left top right bottom)
    (list left bottom (- right left) (- top bottom)))))
;;; <window:Procedures>=
(define (bcoords->edges coords)
  (match coords
  ((x y w h)
    (list x (+ y h) (+ x w) y))))
;;; The best way I can think to tile and scale all these windows is like
;;; this.  Let's use a normalized bounded coordinates for the internal
;;; windows.  This way the frame size can change and the pixel edges can
;;; be recomputed.
;;;
;;; \begin{figure}
;;;   \centering
;;; %  \includegraphics[scale=0.75]{window-diagram.pdf}
;;;   \caption[Window Diagram]{\label{window-diagram}Window $A$ can be
;;;     fully described by two vectors: its origin $\bv o_a = (ox, oy)$
;;;     and its end $\bv e_a = (w_a, h_a)$.}
;;; \end{figure}
;;;
;;;
;;; Imagine the frame has a width $W$ and a height H.  My root window has
;;; the bounded coordinates \verb|(0 0 1 1)|.  When I call
;;; \verb|window-pixel-coords| on it, it will return \verb|(0 0 W H)|.
;;;
;;; Consider the case where my root window is split vertically in half.
;;; My root window would be an internal window with the same bounded
;;; coordinates as before.  The top child, however, will have its pixel
;;; bounded coordinates as \verb|(0 (/ H 2) W (/ H 2)|. And the bottom
;;; child will have \verb|(0 0 W (/ H 2))|.
;;;
;;; One way to think of this is every \verb|<window>| takes up all its
;;; space; intrinsically, they are all set to \verb|(0 0 1 1)|.  The trick
;;; is each \verb|<internal-window>| divides up the space recursively.  So
;;; the internal window in the preceding example that was split
;;; vertically, it passes \verb|0 .5 1 .5| to the top child and
;;; \verb|0 0 1 .5|.
;;;
;;; When the root window, or frame in Emacs parlance, is resized, we want
;;; each windows by default to resize proportionately.  The windows will
;;; be tiled; therefore, it seems appropriate to use the unit of
;;; proportions as our representation over pixels. There will be some
;;; windows that will have a size of a particular pixel size, like the
;;; minibuffer window.  A little bit of specialization to maintain a
;;; particular pixel height will require some callbacks or hooks.
;;;
;;; \subsection{Overriding switch-to-buffer}
;;;
;;; When the user switches to a buffer, then the current window should be
;;; switched to that window.  It'd be preferrable to use an advice
;;; mechanism, but I haven't finished writing that module yet, so we'll
;;; have to settle for something a little more clunky.
;;;
;;;
;;; <window:Procedures>=
(let ((old-func switch-to-buffer))
  (set! switch-to-buffer
        (lambda-cmd args
                    (let ((result (apply old-func args)))
                      (format #t "Setting current window to buffer ~a~%" (current-buffer))
                      (set! (window-buffer current-window) (current-buffer))
                      result))))
;;; <window:Procedures>=
(define-method (window-clone (window <window>))
  (shallow-clone window))
;;; <window:Procedures>=
(define-public (selected-window)
  current-window)
;;; If the internal window size is changed, we want to update the sizes of
;;; its children.  Also, normally we'd only need to keep one matrix and
;;; just invert it as necessary; however, I haven't written a matrix
;;; solver routine, so I'm just going to construct the matrix and its
;;; inverse.  (I wish guile-num were around.)

;;; <window:Procedures>=
(define-method (update-window (window <internal-window>))
 #f
 (when #f
   (let ((children (window-children window)))
      (if (eq? (orientation window) 'vertical)
      ;;; <window:Update vertical window.>=
      #t
      ;;; <window:Update horizontal window.>=
      #t))))
;;; \subsection{Window List}
;;;
;;;
;;; <window:Procedures>=
(define-method (window-tree (w <internal-window>))
  (map window-tree (window-children w)))

(define-method (window-tree (w <window>))
  w)

;;; <window:Procedures>=
(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define* (window-list #:optional (w root-window))
  (flatten (window-tree w)))
;;; \subsection{Split Window}
;;;
;;; Be careful with \verb|deep-clone|. If you deep clone one window that
;;; has references to other windows, you will clone entire object graph.
;;;
;;;
;;; <window:Commands>=
(define-interactive (split-window #:optional
                     (window (selected-window))
                     (size 0.5)
                     (side 'below))
  (define (substitute x y)
    "Returns a function that will substitute x for y when given x."
    (lambda (z)
     (if (eq? z x)
         y
         z)))
  (let* ((original-parent (window-parent window))
         (new-child (window-clone window))
         (internal-window (make <internal-window>
                                #:window-children (list window new-child)
                                #:size size
                                #:orientation (if (memq side '(below above))
                                                  'vertical
                                                  'horizontal))))
    (set! (window-parent internal-window) original-parent)
    (set! (window-parent window)    internal-window)
    (set! (window-parent new-child) internal-window)
    (when original-parent
     (set! (window-children original-parent)
           (map (substitute window internal-window)
                (window-children original-parent))))
    (run-hook window-configuration-change-hook original-parent)
    (update-window internal-window)
  internal-window))
;;; \section{Window Commands}
;;;
;;;
;;; <window:Commands>=
(define-interactive (split-window-below #:optional (size .5))
  (split-window (selected-window) size 'below))
;;; <window:Commands>=
(define-interactive (split-window-right #:optional (size .5))
  (split-window (selected-window) size 'right))
;;; <window:Commands>=
(define-interactive (delete-window #:optional (window (selected-window)))
  (let ((p (window-parent window)))
    ;; Only delete if it has a parent.
    (when p
      (let* ((children (window-children p))
             (new-children (delq window children)))
        (set! (window-children p) new-children)
        (set! current-window (car new-children))
        (run-hook window-configuration-change-hook p)
        ;; XXX We will want to divest ourselves of any internal-windows
        ;; that only contain one child. Not sure if we want to do that here
        ;; or in another method though.
        ;;(if (= 1 (length new-children))
        ;;    (car new-children)
        ;;    (begin
        ;;      #t))
))))

(define-interactive (delete-other-windows #:optional (window (selected-window)))
  (set! root-window (make <internal-window> #:window-children (list window)))
  (set! current-window window)
  (run-hook window-configuration-change-hook root-window))

(define-interactive (other-window #:optional (count 1))
  (let* ((lst (window-list root-window))
         (index (member-ref current-window lst)))
    (set! current-window (list-ref lst (modulo (+ index count) (length lst))))))
;;; \section{Window Key Bindings}
;;;
;;; It will come as no surprise that these key bindings will mimic the
;;; behavior of Emacs.
;;;
;;;
;;; <window:Key bindings>=
(define-key global-map "C-x 0" 'delete-window)
(define-key global-map "C-x 1" 'delete-other-windows)
(define-key global-map "C-x 2" 'split-window-below)
(define-key global-map "C-x 3" 'split-window-right)

(define-key global-map "C-x o" 'other-window)

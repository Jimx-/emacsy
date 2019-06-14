;;; \subsection*{File Layout}                                               
;;;                                                                         
;;;                                                                         
;;; <file:kbd-macro.scm>=                                                   
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
(define-module (emacsy kbd-macro)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (emacsy util)
  #:use-module (emacsy event)
  #:use-module (emacsy command)
  #:use-module (emacsy keymap)
  #:use-module (emacsy klecl)
  #:use-module (emacsy block))


;;; <kbd-macro:state>=                                                      
(define-public defining-kbd-macro?  #f)
(define-public last-kbd-macro '())
;;; <kbd-macro:state>=                                                      
(define-public executing-kbd-macro? #f)
(define-public kbd-macro-termination-hook (make-hook))
;;; <kbd-macro:state>=                                                      
(define-public executing-temporal-kbd-macro-hook (make-hook 1))
;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-                     
;;; \section{Keyboard Macro Module}                                         
;;;                                                                         
;;;                                                                         
;;; \epigraph{...}{...}                                                     
;;;                                                                         
;;; We will now add a keyboard macro facility familiar to Emacs users.  We  
;;; hook into the [[read-event]] procedure using a hook.                    
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
;; XXX This also may record the key event that stops the keyboard
;; macro, which it shouldn't.
(define (kbd-read-event-hook event)
  (when defining-kbd-macro?
      (message "RECORDING ~a" event)
      (cons! event last-kbd-macro)))
;;; \subsection{kmacro-start-macro}                                         
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
(define-interactive (kmacro-start-macro)
  (set! last-kbd-macro '())
  (set! defining-kbd-macro? #t))
;;; \subsection{kmacro-end-macro}                                           
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
(define-interactive (kmacro-end-macro)
  (set! defining-kbd-macro? #f))
;;; \subsection{kmacro-end-and-call-macro}                                  
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
(define-interactive (kmacro-end-and-call-macro)
  (if defining-kbd-macro?
      (kmacro-end-macro))
  (execute-kbd-macro last-kbd-macro))
;;; \subsection{execute-kbd-macro}                                          
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
(define-interactive 
  (execute-kbd-macro #:optional 
                     (kbd-macro last-kbd-macro)
                     (count 1) (loopfunc #f))
  (let ((orig-event-queue event-queue)
        (new-event-queue  (make-q)))
    (for-each (lambda (x) 
               (enq! new-event-queue x)) 
             (reverse kbd-macro))
    (in-out-guard
     (lambda ()
       (set! event-queue new-event-queue)
       (set! executing-kbd-macro? #t))
     (lambda () 
       (command-loop (lambda args (not (q-empty? event-queue)))))
     ;; Turn off the executing-kbd-macro?.
     (lambda () 
       (set! executing-kbd-macro? #f)
       (set! event-queue orig-event-queue)
       (run-hook kbd-macro-termination-hook)))))
;;; \subsection{execute-temporal-kbd-macro}                                 
;;;                                                                         
;;; In addition to regular keyboard macros, Emacsy can execute keyboard     
;;; macros such that they reproduce the keys at the same pace as they were  
;;; recorded.                                                               
;;;                                                                         
;;;                                                                         
;;; <kbd-macro:procedure>=                                                  
(define-interactive 
  (execute-temporal-kbd-macro #:optional (kbd-macro last-kbd-macro)) 
  (in-out
   (set! executing-kbd-macro? #t)
   (let* ((start-time (emacsy-time))
          (macro-start-time (time (last kbd-macro))))
     (let loop ((macro (reverse kbd-macro)))
       (when (not (null? macro))
         (block-until (lambda () 
                       (let ((duration (- (emacsy-time) start-time) ))
                         (run-hook executing-temporal-kbd-macro-hook duration)
                         (>= duration
                             (- (time (car macro)) macro-start-time)))))
         (emacsy-event (car macro))
         (loop (cdr macro)))))
   (begin 
     (set! executing-kbd-macro? #f)
     (run-hook kbd-macro-termination-hook))))
;;; <kbd-macro:process>=                                                    
;; How do I ensure this only happens once?
(add-hook! read-event-hook kbd-read-event-hook)

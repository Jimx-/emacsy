;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-                     
;;; \subsubsection{Most Recently Used Stack}                                
;;; The buffers are kept in a most recently used stack that has the         
;;; following operators: add!, remove!, contains?, recall!, and list.       
;;;                                                                         
;;;                                                                         
;;; <file:mru-stack.scm>=                                                   
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
(define-module (emacsy mru-stack)
  #:use-module (ice-9 q)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:export (<mru-stack>
            mru-add!
            mru-remove!
            mru-recall!
            mru-set!
            mru-ref
            mru-empty?
            mru-contains?
            mru-next!
            mru-prev!
            mru-list))

;;; <mru-stack:class>=                                                      
(define-class <mru-stack> ()
  (queue #:accessor q #:init-thunk (lambda () (make-q)))
  (index #:accessor index #:init-value 0))
;;; <mru-stack:procedure>=                                                  
(define-method (write (obj <mru-stack>) port)
;  (write (string-concatenate (list "#<mru-stack '" (buffer-name obj) "'>")) port)
  (format port "<mru-stack ~a>" (mru-list obj)))
;;; <mru-stack:procedure>=                                                  
(define-method (mru-add! (s <mru-stack>) x)
  (q-push! (q s) x))
(define-method (mru-remove! (s <mru-stack>) x)
  (let ((orig-x (mru-ref s)))
    (q-remove! (q s) x)
    (if (not (eq? orig-x x))
        (mru-set! s orig-x))))
(define-method (mru-recall! (s <mru-stack>) x)
  (q-remove! (q s) x)
  (q-push! (q s) x)
  (set! (index s) 0)
  (mru-list s))
(define-method (mru-set! (s <mru-stack>) x)
  ;; Should this add the buffer if it's not already there? No.
  (if (mru-empty? s)
      #f
      (let ((i (member-ref x (mru-list s))))
        (if i
            (begin (set! (index s) i)
                   #t)
            (begin (mru-next! s)
                   #f)))))
(define-method (mru-ref (s <mru-stack>))
  (and (not (mru-empty? s))
       (list-ref (mru-list s) (index s))))
(define-method (mru-list (s <mru-stack>))
  (car (q s)))
(define-method (mru-empty? (s <mru-stack>))
  (q-empty? (q s)))
(define-method (mru-contains? (s <mru-stack>) x)
  (memq x (mru-list s)))
;;; The order of the elements may not change yet the index may be moved     
;;; around.                                                                 
;;;                                                                         
;;;                                                                         
;;; <mru-stack:procedure>=                                                  
(define-method (mru-next! (s <mru-stack>) count)
  (when (not (mru-empty?  s))
   (set! (index s) 
         (modulo (+ (index s) count) 
                 (length (mru-list s))))
   (mru-ref s)))
(define-method (mru-prev! (s <mru-stack>) count)
  (mru-next! s (- count)))
(define-method (mru-prev! (s <mru-stack>))
  (mru-prev! s 1))
(define-method (mru-next! (s <mru-stack>))
  (mru-next! s 1))

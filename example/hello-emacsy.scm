;; We generate the file @file{example/hello-emacsy.c.x} by running the
;; command: @code{guile-snarf example/hello-emacsy.c}.  Emacsy can now
;; access and alter the application's internal state.
;;.

;; @subsection Changing the UI Now let's use these new procedures to
;; create interactive commands and bind them to keys by changing our
;; config file @file{example/hello-emacsy.scm}.
(use-modules (emacsy emacsy))

;;.
(define-interactive (incr-counter #:optional (n (universal-argument-pop!)))
 "Increment the counter."
 (set-counter! (+ (get-counter) n)))

;;.
(define-interactive (decr-counter #:optional (n (universal-argument-pop!)))
 "Decrement the counter."
 (set-counter! (- (get-counter) n)))

;; Bind @var{inc-counter} to @code{=}.
(define-key global-map "=" 'incr-counter)
;; Bind @var{inc-counter} to @code{-}.
(define-key global-map "-" 'decr-counter)

;; We can now hit @verb{|-|} and @verb{|=|} to decrement and increment the
;; @var{counter}. This is fine, but what else can we do with it?  We could
;; make a macro that increments 5 times by hitting
;; @verb{|C-x ( = = = = = C-x )|}, then hit @verb{|C-e|} to run that macro.
(set! debug-on-error? #t)

;; Let's implement another command that will ask the user for a number to
;; set the counter to.
;;.

;; Now we can hit @verb{|M-x change-counter|} and we'll be prompted for
;; the new value we want.  There we have it.  We have made the simplest
;; application ever more @emph{Emacs-y}.
(define-interactive (change-counter)
 "Change the counter to a new value."
 (set-counter!
   (string->number
     (read-from-minibuffer
       "New counter value: "))))

;; @subsection Changing it at Runtime
;;
;; We can add commands easily by changing and reloading the file.  But
;; we can do better.  Let's start a REPL we can connect to.
;; @file{example/hello-emacsy.scm}.
;;.

;; @example
;; (use-modules (system repl server))
;; (spawn-server)
;; @end example
;; Start a server on port 37146.
;;.
(use-modules (system repl server))
(spawn-server)

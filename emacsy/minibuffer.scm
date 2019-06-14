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
(define-module (emacsy minibuffer)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (ice-9 gap-buffer)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41) ;; streams
  #:use-module (system vm program)
  #:use-module (oop goops)
  #:use-module (string completion)

  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy event)
  #:use-module (emacsy buffer)
  #:use-module (emacsy command)
  #:use-module (emacsy block)
  #:use-module (emacsy klecl)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy agenda)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy cursor-list))


;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-
;;;
;;; \section{Minibuffer}
;;;
;;; \epigraph{...}{...}
;;;
;;; The minibuffer provides a rich interactive textual input system.  It
;;; offers \verb|TAB| completion and history.  The implementation of it
;;; inherits from the [[<text-buffer>]].
;;;
;;;
;;; <minibuffer:class>=
(define-class-public <minibuffer> (<text-buffer>)
  (prompt #:accessor minibuffer-prompt #:init-form "")
  (message #:accessor minibuffer-message-string #:init-form ""))
;;; We define a keymap with all the typical self-insert-commands that
;;; would be expected in an editable buffer.\todo{This should probably be
;;;   defined in the buffer module since it is general.}
;;;
;;;
;;; <minibuffer:state>=
(define-public minibuffer-local-map
  (let ((keymap (make-keymap)))
    (char-set-for-each
     (lambda (c)
       (let ((event (make <key-event>
                      #:command-char c)))
         (define-key keymap (list (event->kbd event))
           'self-insert-command)))
     (char-set-delete
      (char-set-intersection char-set:ascii char-set:printing)
      #\vtab #\page #\space #\nul))
    keymap))
;;; We instantiate the [[<minibuffer>]] class into the global variable
;;; [[minibuffer]].
;;;
;;;
;;; <minibuffer:state>=
(define-public minibuffer
  (make <minibuffer> #:keymap minibuffer-local-map #:name "*minibuffer-1*"))
;;; <minibuffer:state>=
(define-public emacsy-display-minibuffer? #f) ;; or the echo area
;;; <minibuffer:state>=
(define-public minibuffer-message-timeout 5)
(define-public ticks-per-second #f)
(define minibuffer-message-modified-tick 0)
;;; <minibuffer:state>=
(define minibuffer-reading? #f)
;;; <minibuffer:state>=
(define *nth-match* 0)

;;; <minibuffer:state>=
(define minibuffer-completion-table (make-fluid '()))
(define minibuffer-completion-predicate (make-fluid (const #t)))
(define minibuffer-completion-confirm #f)
(define minibuffer-completion-exit-commands '())
;;; <minibuffer:state>=
(define-public minibuffer-history #f)
(define history-symbol-map (make-hash-table))
;;; The minibuffer has a prompt, but we want it to behave generally like
;;; any other text buffer.  So let's implement the procedures:
;;; [[buffer-string]], [[point]], [[point-min]], [[point-max]], and
;;; [[goto-char]].
;;;
;;; When we show the minibuffer, we'll show the prompt, the contents (user
;;; editable), and the minibuffer-message if applicable.
;;;
;;;
;;; <minibuffer:procedure>=
(define-method (buffer-string (buffer <minibuffer>))
  (string-concatenate (list
                       (minibuffer-prompt buffer)
                       (minibuffer-contents buffer)
                       (minibuffer-message-string buffer))))
;;; <minibuffer:procedure>=
(define*-public (minibuffer-contents #:optional (buffer minibuffer))
  (gb->string (gap-buffer buffer)))

(define*-public (delete-minibuffer-contents #:optional (buffer minibuffer))
  (gb-erase! (gap-buffer buffer)))
;;; For the point methods, we're going to make [[(goto-char 1)]] the
;;; beginning of the prompt, but [[(point-min)]] where the user editable
;;; content starts.  Basically, it should be as though it were a regular
;;; buffer that has been narrowed.
;;;
;;;
;;; <minibuffer:procedure>=
(define-method (point-min (buffer <minibuffer>))
  (+ (next-method) (string-length (minibuffer-prompt buffer))))

(define-method (point (buffer <minibuffer>))
  (+ (next-method) (string-length (minibuffer-prompt buffer))))

(define-method (point-max (buffer <minibuffer>))
  (+ (next-method) (string-length (minibuffer-prompt buffer))))
;;; For [[goto-char]] we just undo that thing.\todo{If the prompt changes,
;;;   the point should be adjusted manualy.}
;;;
;;;
;;; <minibuffer:procedure>=
(define-method (goto-char point (buffer <minibuffer>))
  (gb-goto-char (gap-buffer buffer)
                (- point (string-length (minibuffer-prompt buffer)))))
;;; One can add a message to the minibuffer that can act as an interactive
;;; help or show possible completions.  The message will only last until
;;; the next command is executed.
;;;
;;;
;;; <minibuffer:procedure>=
(define* (seconds->ticks seconds #:optional (default-ticks #f))
  "Converts seconds to number of ticks, if such a conversion is
available. Otherwise returns default-ticks."
  (if ticks-per-second
      (* seconds ticks-per-second)
      default-ticks))

(define-public (minibuffer-message string . args)
  (set! (minibuffer-message-string minibuffer)
        (apply format #f string args))
  (incr! minibuffer-message-modified-tick)
  (agenda-schedule (let ((my-tick minibuffer-message-modified-tick))
                     (lambda ()
                       (when (= my-tick minibuffer-message-modified-tick)
                         (set! (minibuffer-message-string minibuffer) ""))))
                       (seconds->ticks minibuffer-message-timeout 1)))
;;; \subsection{read-from-minibuffer}
;;;
;;;
;;; <minibuffer:procedure>=
(define*-public (read-from-minibuffer prompt #:optional
                                      (initial-contents #f)
                                      #:key
                                      (read #f)
                                      (keymap minibuffer-local-map)
                                      (history (what-command-am-i?)))
  "history can be #f, a symbol, or a <cursor-list>."
  (define (read-from-minibuffer-internal prompt read)
    (when minibuffer-reading?
      (minibuffer-message
       " [Command attempted to use minibuffer while in minibuffer.]")
      (throw 'quit-command 'already-in-minibuffer))
    (when history
      (cond
       ((symbol? history)
        (let ((entry (hashq-ref history-symbol-map history #f)))
          (unless entry
            (set! entry (make-history))
            (hashq-set! history-symbol-map history entry))
          (set! minibuffer-history entry)))
       ((cursor-list? history)
          (set! minibuffer-history history))
       (else
        (scm-error 'invalid-argument "read-from-minibuffer" "Expecting #f, a symbol, or a <cursor-list> for history argument; instead got ~a." (list history) #f))))
    (history-insert! minibuffer-history "")
   (emacsy-log-debug "Switching to minibuffer now.")
   (switch-to-buffer minibuffer)
   (delete-minibuffer-contents minibuffer)
   (with-buffer minibuffer
    (when initial-contents
      (insert initial-contents))
    (goto-char (point-min)))
   (set! (minibuffer-prompt minibuffer) (or prompt ""))
   (in-out
    (set! minibuffer-reading? #t)
    (let ((canceled? #f))
      (catch
        'quit-command
        (lambda ()
          (while minibuffer-reading?
            (primitive-command-tick)))
        (lambda (key . args)
          (emacsy-log-debug "MINIBUFFER CANCELED!\n")
          (set! canceled? #t)))
      (if canceled?
          (begin
            (if (eq? (current-buffer) minibuffer)
                (switch-to-buffer last-buffer))
            (throw 'quit-command 'quit-read-from-minibuffer))
          (begin
            (history-set! minibuffer-history (minibuffer-contents))
            (cursor-right! minibuffer-history)
            (minibuffer-contents))))
    (set! minibuffer-reading? #f)))

  (let ((original-keymap #f)
        (original-history #f))
    (in-out
     (begin (set! original-keymap (local-keymap minibuffer))
            (set! (local-keymap minibuffer) keymap)
            (set! original-history minibuffer-history))
     (read-from-minibuffer-internal prompt read)
     (begin
       (set! (local-keymap minibuffer) original-keymap)
       (set! minibuffer-history original-history))
     '(quit-command keyboard-quit))))
;;; <minibuffer:procedure>=
(define (readline-completer->stream completer string)
  (define iter
    (stream-lambda (f)
                   (let ((result (f)))
                     (if (stream-null? result)
                         result
                         (stream-cons result (iter f))))))
  (let ((first (completer string #f)))
    (if first
        (stream-cons
         first
         (iter
          (lambda ()
            (let ((result (completer string #t)))
              (if result
                  result
                  stream-null)))))
        stream-null)))
;;; <minibuffer:procedure>=
(define (required-arguments proc)
  (assoc-ref (program-arguments-alist proc) 'required))
(define (readline-completer? proc)
  (let ((req (length (required-arguments proc))))
    (= req 2)))

;;; <minibuffer:procedure>=
(define*-public
  (try-completion string collection #:optional (predicate (const #t)))

  (if (procedure? collection)
      (if (readline-completer? collection)
          (try-completion
           string
           (stream->list (readline-completer->stream collection string))
           predicate)
          (collection string predicate #f))
      (let ((completer (collection->completer collection predicate)))
        (receive (completions expansion exact? unique?)
            (complete completer string)
          expansion))))
;;; <minibuffer:procedure>=
(define*-public
  (all-completions string collection #:optional (predicate (const #t)))
  (if (procedure? collection)
      (if (readline-completer? collection)
          (all-completions
           string
           (stream->list
            (readline-completer->stream collection string))
           predicate)
          (collection string predicate #t))
   (let ((completer (collection->completer collection predicate)))
     (receive (completions expansion exact? unique?)
         (complete completer string)
       completions))))
;;; <minibuffer:procedure>=
(define*-public
  (collection->completer collection #:optional (predicate (const #t)))
  (if (is-a? collection <string-completer>)
      collection

      (let ((completer (make <string-completer>)))
        (add-strings! completer (filter predicate collection))
        completer)))
;;; <minibuffer:procedure>=
(define (rotate-list lst nth)
  (let* ((nth (modulo nth (length lst)))
         (tail (list-tail lst nth))
         (head (list-head lst nth)))
    (append tail head)))
;;; <minibuffer:procedure>=
(define*-public
  (completing-read prompt collection
                   #:key
                   (predicate (const #t))
                   (require-match? #f)
                   (initial-input #f)
                   (history (what-command-am-i?))
                   (to-string #f))

;; XXX implement require-match?
  (define (completing-read* collection*)
    (with-fluids ((minibuffer-completion-table collection*)
                  (minibuffer-completion-predicate predicate))
      (read-from-minibuffer prompt initial-input
                            #:keymap minibuffer-local-completion-map
                            #:history history)))
  (cond
   (to-string
    (receive (to-string* from-string*) (object-tracker to-string)
      (from-string* (completing-read* (map to-string* collection)))))
   (else
    (completing-read* collection))))
;;; \subsection{Filename Lookup}
;;;
;;; \subsubsection{New}
;;;
;;; We can do filename lookups by using the readline tab completion facilities.
;;;
;;;
;;; <minibuffer:procedure>=
(define-public (apropos-module rgx module)
  "Return a list of accessible variable names for a given module."
  (apropos-fold (lambda (module name var data)
                  (cons name data))
                '()
                rgx
                (apropos-fold-accessible module)))

(define-public command-completion-function
  (let ((completions '()))
    (lambda (text cont?)
      (if (not cont?)
          (set! completions
                (map symbol->string
                     (apropos-module
                      (string-append "^" (regexp-quote text))
                      (module-command-interface (resolve-module '(emacsy core)))))))
      (if (null? completions)
          #f
          (let ((retval (car completions)))
            (begin (set! completions (cdr completions))
                   retval))))))
;;; \subsubsection{Old}
;;;
;;; We want to be able to look up filenames.
;;;
;;;
;;; <minibuffer:procedure>=
(define (files-in-dir dirname)
  (let ((dir (opendir dirname))
        (filenames '()))
    (let loop ((filename (readdir dir)))
      (when (not (eof-object? filename))
        (cons! filename filenames)
        (loop (readdir dir))))
    (closedir dir)
    (sort-filenames filenames)))

(define (make-filename dir name)
  (format #f "~a~a" (if (slash-suffix? dir)
                        dir
                        (format #f "~a/" dir)) name))

(define (sort-filenames lst)
  (sort lst string<))

(define (files-in-parent-dir filename)

  (let* ((dir (my-dirname filename))
         ;(base (basename filename))
         (filenames (files-in-dir dir)))
    (sort-filenames
     (if (or (string-prefix? dir filename)
             (string-prefix? filename dir)) ;; the directory is a prefix of
         ;; the string--good!
         (map (compose canonize-filename (cut make-filename dir <>)) filenames)
         (map canonize-filename filenames)))))

(define (directory? filename)
  (and (access? filename F_OK)
       (eq? 'directory (stat:type (stat filename)))))

(define (slash-suffix? name)
  (eq? #\/ (string-ref name (1- (string-length name)))))

(define (canonize-filename name)
  (if (directory? name)
      (if (slash-suffix? name)
          name
          (format #f "~a/" name))
      name))

(define (my-dirname name)
  (if (directory? name)
      (canonize-filename name)
      (dirname name)))

(define (file-name-completer string predicate all?)
    (if all?
        (all-completions string (files-in-parent-dir string) predicate)
        (try-completion string (files-in-parent-dir string) predicate)))

(define (dot-directory? name)
  (or ;(string=? "." name)
      ;(string=? ".." name)
      (string-suffix? "./" name)
      (string-suffix? "../" name)))

(define no-dot-files (negate dot-directory?))

(define*-public
  (read-file-name prompt #:key
                  (dir #f)
                  (default-file-name #f)
                  (initial #f)
                  (predicate no-dot-files)
                  (history (what-command-am-i?)))
  (completing-read prompt
                   filename-completion-function
                   #:predicate predicate
                   #:history history))
;;; \subsection{Minibuffer History}
;;;
;;;
;;; <minibuffer:procedure>=
(define*-public (make-history #:optional (list '()) (index #f))
  (make-cursor-list list (or index (length list))))

(set! minibuffer-history (make-history))

(define-public (history-insert! history value)
  (cursor-right-insert! history value)
  ;;(fluid-set! minibuffer-history
  ;;            (list-insert! (fluid-ref minibuffer-history)
  ;;                       index
  ;;                       value))
  )

(define-public (history-ref history)
  (if (cursor-right? history)
   (cursor-right-ref history)
   #f)
  ;;(list-ref (fluid-ref minibuffer-history) index)
  )

(define-public (history-set! history value)
      (cursor-right-set! history value)
      ;;(if (cursor-right? history)
      ;;(cursor-right-set! history value)
      ;;(cursor-right-insert! history value))
  ;;(let ((lst (fluid-ref minibuffer-history)))
  ;;  (list-set! lst index value)
  ;;  (fluid-set! minibuffer-history lst))
  value)
;;; <minibuffer:command>=
(define-interactive (exit-minibuffer)
  (set! minibuffer-reading? #f)
  (switch-to-buffer last-buffer))
;;; <minibuffer:command>=
(define-interactive (minibuffer-complete)
  (with-buffer
   minibuffer
   (let* ((contents (substring (minibuffer-contents) 0 (- (point) (point-min))))
          (expansion (try-completion
                      contents
                      (fluid-ref minibuffer-completion-table)
                      (fluid-ref minibuffer-completion-predicate)))
          (completions (all-completions
                        contents
                        (fluid-ref minibuffer-completion-table)
                        (fluid-ref minibuffer-completion-predicate))))
     (format #t "contents = ~a, expansion = ~a, completions = ~a ~%" contents expansion completions)
     (while (not (= (point) (point-min)))
       (delete-backward-char))
                                        ;(goto-char (point-min))
                                        ;(kill-line)
     (insert expansion)
     (cond
      ((= 0 (length completions))
       (minibuffer-message " [No match]"))
      ((= 1 (length completions))
       (minibuffer-message " [Sole completion]"))
      ((> (length completions) 1)
       (minibuffer-message
        (string-concatenate
         (list "{"
               (string-join (rotate-list completions *nth-match*) " | ")
               "}"))))))))
;;; <minibuffer:command>=
(define-interactive (next-match)
  (incr! *nth-match*)
  (minibuffer-complete))

(define-interactive (previous-match)
  (decr! *nth-match*)
  (minibuffer-complete))

;;; <minibuffer:command>=
(define-interactive (minibuffer-complete-word)
  ;; This should only complete a word.
  (minibuffer-complete))

(define-interactive (minibuffer-completion-help)
  ;; This should only complete a word.
  (message "minibuffer-complete-help NYI")
  #f)
;;; Some commands for manipulating the minibuffer history.
;;;
;;;
;;; <minibuffer:command>=
(define-interactive (previous-history-element #:optional (n 1))
  (define (previous-history-element* n)
    (cond
    ((> n 0)
     (if (cursor-left? minibuffer-history)
         (begin
           (cursor-left! minibuffer-history)
           (previous-history-element* (1- n)))
         (begin (minibuffer-message " [Beginning of history; no preceding item]")
                ;;(previous-history-element 0)
                )))
    ((< n 0)
     (if (cursor-right? minibuffer-history 2)
         (begin
           (cursor-right! minibuffer-history)
           (previous-history-element* (1+ n)))
         (begin (minibuffer-message " [End of history; no default available]")
                ;;(previous-history-element 0)
                )))
    ((= n 0)
     #f)))
  ;;
  (history-set! minibuffer-history (minibuffer-contents))
  (previous-history-element* n)
(with-buffer minibuffer (goto-char (point-min)))
(delete-minibuffer-contents minibuffer)
(insert (history-ref minibuffer-history))
(format #t "minibuffer-history ~a~%" minibuffer-history)
(history-ref minibuffer-history)

  ;;(let* ((i (fluid-ref minibuffer-history-index))
  ;;       (j (+ i n))
  ;;       (history (fluid-ref minibuffer-history)))
  ;;  (pretty-print history)
  ;;  (pretty-print i)
  ;;  (pretty-print j)
  ;;  (cond ((< j 0)
  ;;         )
  ;;        ((>= j (length history))
  ;;         (minibuffer-message " [Beginning of history; no preceding item]"))
  ;;        (else
  ;;         (history-set! i (minibuffer-contents))
  ;;         (with-buffer minibuffer
  ;;         (goto-char (point-min)))
  ;;         (delete-minibuffer-contents minibuffer)
  ;;         (insert (list-ref history j))
  ;;         (fluid-set! minibuffer-history-index j))))
     )

(define-interactive (next-history-element #:optional (n 1))
  (previous-history-element (- n)))
;;; We want to be able to move around the buffer as well.
;;;
;;;
;;; <minibuffer:keymap>=
(define-key minibuffer-local-map "C-f" 'forward-char)
(define-key minibuffer-local-map "M-f" 'forward-word)
(define-key minibuffer-local-map "C-b" 'backward-char)
(define-key minibuffer-local-map "M-b" 'backward-word)
(define-key minibuffer-local-map "DEL" 'delete-backward-char)
(define-key minibuffer-local-map "SPC" 'self-insert-command)
(define-key minibuffer-local-map "-"   'self-insert-command)
(define-key minibuffer-local-map "C-a" 'move-beginning-of-line)
(define-key minibuffer-local-map "C-e" 'move-end-of-line)
(define-key minibuffer-local-map "C-k" 'kill-line)
(define-key minibuffer-local-map "C-d" 'forward-delete-char)
(define-key minibuffer-local-map "RET" 'exit-minibuffer)
(define-key minibuffer-local-map "M-n" 'next-history-element)
(define-key minibuffer-local-map "M-p" 'previous-history-element)
(define-key minibuffer-local-map "C-g" 'keyboard-quit)
;;; <minibuffer:keymap>=
(define minibuffer-local-completion-map
  (let ((kmap (make-keymap minibuffer-local-map)))
    ;(define-key kmap "SPC" 'minibuffer-complete-word)
    (define-key kmap "TAB" 'minibuffer-complete)
    (define-key kmap "?"   'minibuffer-completion-help)
    (define-key kmap "C-s" 'next-match)
    (define-key kmap "C-r" 'previous-match)))
;;; Whenever the minibuffer is being used, we want to show it instead of
;;; the echo area, so we add some hooks.
;;;
;;;
;;; <minibuffer:process>=
(add-hook! (buffer-enter-hook minibuffer)
           (lambda ()
             (emacsy-log-debug "Enter minibuffer!\n")
             (set! emacsy-display-minibuffer? #t)))

(add-hook! (buffer-exit-hook  minibuffer)
           (lambda ()
             (emacsy-log-debug "Exit minibuffer!\n")
             (set! emacsy-display-minibuffer? #f)))

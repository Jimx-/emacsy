;;; Long-time Emacs users will be familiar with this idea, but new Emacs    
;;; users may not be.  For instance, when a user hits the 'a' key, then an  
;;; 'a' is inserted into their document.  Let's pull apart the functions    
;;; to see what that actually looks like with respect to the KLECL.         
;;;                                                                         
;;;                                                                         
;;; \begin{verbatim}                                                        
;;; > (read-key)                                                            
;;; #\a                                                                     
;;; > (lookup-key #\a)                                                      
;;; self-insert-command                                                     
;;; > (execute-command 'self-insert-command)                                
;;; #t                                                                      
;;; \end{verbatim}                                                          
;;;                                                                         
;;; Key sequences in Emacs are associated with commands.  The fact that     
;;; each command is implemented in Lisp is an implementation detail and     
;;; not essential to the idea of a KLECL.                                   
;;;                                                                         
;;; Note how flexible the KLECL is: One can build a REPL out of a KLECL,    
;;; or a text editor, or a robot simulator (as shown in the video).  Emacs  
;;; uses the KLECL to create an extensible text editor.  Emacsy uses the    
;;; KLECL to make other applications similarly extensible.                  
;;;                                                                         
;;; \subsection{Goals}                                                      
;;;                                                                         
;;; The goals of this project are as follows.                               
;;;                                                                         
;;; \begin{enumerate}                                                       
;;; \item Easy to embed technically                                         
;;;                                                                         
;;;   Emacsy will use Guile Scheme to make it easy to embed within C and    
;;;   C++ programs.                                                         
;;;                                                                         
;;; \item Easy to learn                                                     
;;;                                                                         
;;;   Emacsy should be easy enough to learn that the uninitiated may easily 
;;;   make parametric changes, e.g., key 'a' now does what key 'b' does     
;;;   and \emph{vice versa}.  Programmers in any language ought to be able  
;;;   to make new commands for themselves.  And old Emacs hands should be   
;;;   able to happily rely on old idioms and function names to change most  
;;;   anything.                                                             
;;;                                                                         
;;; \item Opinionated but not unpersuadable                                 
;;;                                                                         
;;;   Emacsy should be configured with a sensible set of defaults           
;;;   (opinions).  Out of the box, it is not \emph{tabla rasa}, a blank     
;;;   slate, where the user must choose every detail, every time.           
;;;   However, if the user wants to choose every detail, they can.          
;;;                                                                         
;;; \item Key bindings can be modified                                      
;;;                                                                         
;;;   It wouldn't be Emacs-like if you couldn't tinker with it.             
;;;                                                                         
;;; \item Commands can be defined in Emacsy's language or the host          
;;;   language                                                              
;;;                                                                         
;;;   New commands can be defined in Guile Scheme or C/C++.                 
;;;                                                                         
;;; \item Commands compose well                                             
;;;                                                                         
;;;   That is to say, commands can call other commands.  No special         
;;;   arrangements must be considered in the general case.                  
;;;                                                                         
;;; \item A small number of \emph{interface} functions                      
;;;                                                                         
;;;   The core functions that must be called by the embedding application   
;;;   will be few and straightforward to use.                               
;;;                                                                         
;;; \item Bring KLECL to light                                              
;;;                                                                         
;;; \end{enumerate}                                                         
;;;                                                                         
;;; \subsection{Anti-goals}                                                 
;;;                                                                         
;;; Just as important as a project's goals are its anti-goals: the things   
;;; it is not intended to do.                                               
;;;                                                                         
;;; \begin{enumerate}                                                       
;;; \item Not a general purpose text editor                                 
;;;                                                                         
;;;   Emacsy will not do general purpose text editing out of the box,       
;;;   although it will have a minibuffer.                                   
;;;                                                                         
;;; \item Not an Emacs replacement                                          
;;;                                                                         
;;;   Emacs is full featured programmer's text editor with more bells and   
;;;   whistles than most people will ever have the time to fully explore.   
;;;   Emacsy extracts the Emacs spirit of application and UI extensibility  
;;;   to use within other programs.                                         
;;;                                                                         
;;; \item Not an Elisp replacement                                          
;;;                                                                         
;;;   There have been many attempts to replace Emacs and elisp with an      
;;;   newer Lisp dialect.  Emacsy is not one of them.                       
;;;                                                                         
;;; \item Not source code compatible with Emacs                             
;;;                                                                         
;;;   Although Emacsy may adopt some of naming conventions of Emacs, it     
;;;   will not use elisp and will not attempt to be in any way source code  
;;;   compatible with Emacs.                                                
;;;                                                                         
;;; \item Not a framework                                                   
;;;                                                                         
;;;   I will not steal your runloop.  You call Emacsy when it suits your    
;;;   application not the other way around.                                 
;;;                                                                         
;;; \end{enumerate}                                                         
;;;                                                                         
;;; \section{Emacsy Features}                                               
;;;                                                                         
;;; These are the core features from Emacs that will be implemented in Emacsy.
;;;                                                                         
;;; \begin{enumerate}                                                       
;;; \item keymaps                                                           
;;; \item minibuffer                                                        
;;; \item recordable macros                                                 
;;; \item history                                                           
;;; \item tab completion                                                    
;;; \item major and minor modes                                             
;;; \end{enumerate}                                                         
;;;                                                                         
;;;                                                                         
;;;                                                                         
;;; \chapter{The Garden}                                                    
;;;                                                                         
;;; Now for a little entertainment.                                         
;;;                                                                         
;;; \includepdf[pages=-,%addtotoc={1,section,1,The Garden,the-garden},      
;;;   fitpaper=true]{the-garden.pdf}                                        
;;;                                                                         
;;; \chapter{Hello, Emacsy!}                                                
;;;                                                                         
;;; %% \section{Implementation Plan}                                        
;;;                                                                         
;;; %% One could go about implementing a project with the aforementioned    
;;; %% goals in many different ways.  Althought, I am interested in         
;;; %% separating the KLECL from Emacs in particular, for instance I think  
;;; %% implementing a KLECL in \verb|Lua| would be valuable, but I also want
;;; %% more than just the KLECL.  I want to provide an Emacs-like way of    
;;; %% developing applications for developers of all stripes.               
;;;                                                                         
;;; %% \subsection{Implementation Decisions}                                
;;;                                                                         
;;; %% \begin{enumerate}                                                    
;;; %% \item Use GNU Guile Scheme.                                          
;;;                                                                         
;;; %% \item Adopt Emacs naming conventions.                                
;;;                                                                         
;;; %% \item Write as a literate program.                                   
;;;                                                                         
;;; %%   To assist in the goal of making this project understandable enough 
;;; %%   to trust, I plan to use                                            
;;; %%   \verb|nuweb|\footnote{\url{http://nuweb.sourceforge.net/}} and write
;;; %%   this in literate programming fashion where the source code and     
;;; %%   documentation come from the same document.                         
;;;                                                                         
;;; %% %\item Make code available on github                                 
;;; %% \end{enumerate}                                                      
;;;                                                                         
;;; \input{hello-emacsy}                                                    
;;;                                                                         
;;; %\part{Implementation}                                                  
;;; %\chapter{C API}                                                        
;;;                                                                         
;;; \input{_emacsy-c-api}                                                   
;;;                                                                         
;;; \lstset{language=lisp}                                                  
;;; \chapter{KLECL}                                                         
;;;                                                                         
;;; I expounded on the virtues of the Key Lookup Execute Command Loop       
;;; (KLECL) in \ref{klecl-treasure}.  Now we're going to implement a        
;;; KLECL, which requires fleshing out some concepts.  We need events,      
;;; keymaps, and commands.  Let's begin with events.                        
;;;                                                                         
;;; \input{_event}                                                          
;;;                                                                         
;;; \input{_keymap}                                                         
;;;                                                                         
;;; \input{_command}                                                        
;;;                                                                         
;;; \input{_block}                                                          
;;;                                                                         
;;; \input{_klecl}                                                          
;;;                                                                         
;;; \input{_advice}                                                         
;;;                                                                         
;;; \chapter{Emacs-like Personality}                                        
;;;                                                                         
;;; We now want to take our KLECL and implement an Emacs-like UI on top of  
;;; it.  This will include buffers, keyboard macros, and a minibuffer.      
;;;                                                                         
;;; \input{_buffer}                                                         
;;;                                                                         
;;; \input{_minibuffer}                                                     
;;;                                                                         
;;; \input{_core}                                                           
;;;                                                                         
;;; \section{Emacsy Facade}                                                 
;;;                                                                         
;;; So that users of our library don't have to import all of our nicely     
;;; partitioned modules individually, we'll expose a facade module that     
;;; re-exports all of the public interfaces for each module.                
;;;                                                                         
;;;                                                                         
;;; <file:emacsy.scm>=                                                      
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
(define-module (emacsy emacsy)

  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:use-module (emacsy command)
  #:use-module (emacsy mode)
  #:use-module (emacsy buffer)
  #:use-module (emacsy block)
  #:use-module (emacsy klecl)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy core)
  #:use-module (emacsy help))
;;; <emacsy:procedure>=                                                     
(define (re-export-modules . modules)
  (define (re-export-module module)
    (module-for-each 
     (lambda (sym var) 
       ;;(format #t "re-exporting ~a~%" sym)
       (module-re-export! (current-module) (list sym)))
     (resolve-interface module)))
  (for-each re-export-module modules))
;;; <emacsy:process>=                                                       
(re-export-modules 
 '(emacsy util)
 '(emacsy self-doc)
 '(emacsy keymap)
 '(emacsy event)
 '(emacsy mode)       
 '(emacsy buffer)
 '(emacsy coroutine)
 '(emacsy agenda)
 '(emacsy command)
 '(emacsy block)
 '(emacsy klecl)
 '(emacsy kbd-macro)
 '(emacsy minibuffer)
 '(emacsy core)
 '(emacsy help))

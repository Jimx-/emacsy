;;; Emacsy --- An embeddable Emacs-like library using GNU Guile.
;;; Copyright (C) 2019 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Emacsy.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package.  To build and play, run:
;;
;;   guix environment --ad-hoc -l guix.scm guile
;;
;; To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules ((guix licenses) #:prefix license:)
             (guix build-system gnu)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages compression)
             (gnu packages glib)
             (gnu packages gettext)
             (gnu packages gl)
             (gnu packages gnome)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages perl)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tex)
             (gnu packages webkit))

(define %source-dir (dirname (current-filename)))

(package
  (name "emacsy")
  (version "git")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("bzip2" ,bzip2)
     ("guile" ,guile-2.2)
     ("gettext" ,gnu-gettext)
     ("libtool" ,libtool)
     ("perl" ,perl)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)
     ("texlive" ,texlive)))
  (propagated-inputs
   `(("dbus-glib" ,dbus-glib)
     ("guile" ,guile-2.2)
     ("guile-lib" ,guile-lib)
     ("guile-readline" ,guile-readline)
     ("glib-networking" ,glib-networking)
     ("freeglut" ,freeglut)
     ("gssettings-desktop-schemas" ,gsettings-desktop-schemas)
     ("webkitgtk" ,webkitgtk)))
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-before 'configure 'setenv
         (lambda _
           (setenv "GUILE_AUTO_COMPILE" "0")
           #t)))))
  (home-page "https://savannah.nongnu.org/projects/emacsy")
  (synopsis "Embeddable GNU Emacs-like library using Guile")
  (description
   "Emacsy is an embeddable Emacs-like library that uses GNU Guile
as extension language.  Emacsy can give a C program an Emacsy feel with
keymaps, minibuffer, recordable macros, history, tab completion, major
and minor modes, etc., and can also be used as a pure Guile library.  It
comes with a simple counter example using FreeGLUT and browser examples
in C using Gtk+-3 and WebKitGtk.")
  (license license:gpl3+))

SUBDIRS = emacsy

MAINTAINERCLEANFILES = Makefile.in config.h.in 

NOWEB_FILES = line-pragma.nw

NOWEB_PRODUCTS = line-pragma.scm
NOWEB_INCLUDES = $(top_srcdir)/support/noweb/boiler-plate.nw
NOWEB_SCM_INCLUDES = $(top_srcdir)/support/noweb/boiler-plate-scm.nw
NOTANGLE_LISP_FLAGS = -scm $(LINE_PRAGMA_FLAGS) $(MARKUP_FLAGS)
CLEANFILES = $(NOWEB_CLEANFILES)
BUILT_SOURCES = $(NOWEB_BUILT_FILES)
EXTRA_DIST = convenience-lambda.scm cursor-list.scm $(NOWEB_EXTRA_DIST)

SCHEME_FILES = line-pragma.scm convenience-lambda.scm cursor-list.scm
GOBJECTS = $(SCHEME_FILES:%.scm=%.go)

dist_guilemodule_DATA = $(SCHEME_FILES)
guileobject_DATA = $(GOBJECTS)

include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

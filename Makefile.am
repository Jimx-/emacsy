ACLOCAL_AMFLAGS         = -I support/m4

SUBDIRS = src test bin example

EXTRA_DIST = README.md NEWS guix.scm support bin

sign-dist: dist
	gpg -a --output $(distdir).tar.gz.sig --detach-sig $(distdir).tar.gz

verify-sig: $(distdir).tar.gz.sig $(distdir).tar.gz
	gpg --verify $(distdir).tar.gz.sig

# I am using the following guidelines for the clean targets.
# http://ftp.gnu.org/old-gnu/Manuals/automake-1.7.2/html_chapter/automake_14.html
MAINTAINERCLEANFILES = Makefile.in \
libtool \
configure \
aclocal.m4 \
support/build-aux/config.guess \
support/build-aux/config.sub \
support/build-aux/depcomp \
support/build-aux/install-sh \
support/build-aux/ltmain.sh \
support/build-aux/missing \
support/build-aux/test-driver \
support/m4/libtool.m4 \
support/m4/ltoptions.m4 \
support/m4/ltsugar.m4 \
support/m4/ltversion.m4 \
support/m4/lt~obsolete.m4 \
$(distdir) \
$(distdir).tar.gz

untangle:
	@echo first do: guix environment -l guix.scm
	./autogen.sh
	./configure
	make clean
	make
	make check
	git rm -f $$(git ls-files 'src/*.nw' 'support/*.nw')
	git add -f $$(find src -name '*.scm')
	for i in $$(ls -1 src/emacsy/*-test.scm); do mv $$i test/$$(basename $$i -test.scm).scm; done
	rm -rf emacsy
	mkdir -p emacsy
	mv src/emacsy/*.scm emacsy
	mv src/emacsy/emacsy.h emacsy
	mv src/emacsy/emacsy.c emacsy
	mv src/*.scm emacsy
	sed -i -e 's,(define-module (convenience-lambda),(define-module (emacsy convenience-lambda),' emacsy/convenience-lambda.scm
	sed -i -e 's,(define-module (cursor-list),(define-module (emacsy cursor-list),' emacsy/cursor-list.scm
	sed -i -e 's,(define-module (line-pragma),(define-module (emacsy line-pragma),' emacsy/line-pragma.scm
	sed -i -e 's,(convenience-lambda),(emacsy convenience-lambda),e' emacsy/*.scm
	sed -i -e 's,(cursor-list),(emacsy cursor-list),e' emacsy/*.scm
	sed -i -e 's,(line-pragma),(emacsy line-pragma),e' emacsy/*.scm
	git add emacsy/*.scm
	git add test/*.scm
	git rm -r src
	git add -u
	git commit -m 'untangle: run.'
	make -C example/hello-emacsy untangle
	make -C example/emacsy-webkit-gtk untangle

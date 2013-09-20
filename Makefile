SRCDIRS = src demo examples test
TESTDIR = efatest

HTDIR = $(HOME)/haskell
HTPKGS = gnuplot non-empty unique-logic-tf utility

hlint.html: $(SRCDIRS) hlint/efa2.hs
	hlint $(SRCDIRS) --hint=hlint/efa2.hs --report=$@

trim:
	for file in `find $(SRCDIRS) -name "*.hs"`; do \
	  perl -i -p -e 's: +\n:\n:' $$file; \
	done

testgit:
	(export EFA=$$PWD && cd /tmp/ && git clone $$EFA $(TESTDIR) &&
	 cabal install --enable-documentation --disable-shared --disable-library-profiling $(TESTDIR)/ &&
	 rm -r $(TESTDIR)/)

testgit-first:
	(export EFA=$$PWD && cd /tmp/ && git clone $$EFA $(TESTDIR) && cd $(TESTDIR) && \
	 cabal configure --disable-shared --disable-library-profiling)
	make testgit-run

testgit-again:
	(export EFA=$$PWD && cd /tmp/$(TESTDIR)/ && git pull $$EFA)
	make testgit-run

testgit-run:
	(export EFA=$$PWD && cd /tmp/$(TESTDIR)/ && cabal build && cabal haddock && ./dist/build/test-efa/test-efa)

testgit-revert:
	(cd /tmp/$(TESTDIR)/ && git reset HEAD^ && git checkout -f)

htpkg-get:
	(cd $(HTDIR) && \
	   darcs get http://code.haskell.org/gnuplot/ && \
	   darcs get http://code.haskell.org/~thielema/utility/ && \
	   darcs get http://code.haskell.org/~thielema/non-empty/ && \
	   darcs get http://code.haskell.org/~thielema/unique-logic-tf/ )

htpkg-pull:
	(for pkg in $(HTPKGS); do (cd $(HTDIR)/$$pkg && darcs pull -a); done)

htpkg-install:
	for pkg in $(HTPKGS); do echo $(HTDIR)/$$pkg; done | \
	   xargs cabal install .

ghci:
	ghci -i:src:test -Wall -fwarn-incomplete-uni-patterns -fwarn-tabs demo/numericSolving/Main.hs

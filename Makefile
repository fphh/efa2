SRCDIRS = src demo examples test
TESTDIR = efatest

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
	(export EFA=$$PWD && cd /tmp/ && git clone $$EFA $(TESTDIR) && cabal install --enable-documentation --disable-shared --disable-library-profiling $(TESTDIR)/)

testgit-again:
	(export EFA=$$PWD && cd /tmp/$(TESTDIR)/ && git pull $$EFA && cabal build && cabal haddock)

testgit-revert:
	(cd /tmp/$(TESTDIR)/ && git reset HEAD^ && git checkout -f)

ghci:
	ghci -i:src:test -Wall -fwarn-incomplete-uni-patterns -fwarn-tabs demo/numericSolving/Main.hs

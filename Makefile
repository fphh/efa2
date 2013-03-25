SRCDIRS = src demo examples test

hlint.html: $(SRCDIRS) hlint/efa2.hs
	hlint $(SRCDIRS) --hint=hlint/efa2.hs --report=$@

trim:
	for file in `find $(SRCDIRS) -name "*.hs"`; do \
	  perl -i -p -e 's: +\n:\n:' $$file; \
	done

testgit:
	(export EFA=$$PWD && cd /tmp/ && git clone $$EFA efatest && cabal install --enable-documentation --disable-shared --disable-library-profiling efatest/)

testgit-again:
	(export EFA=$$PWD && cd /tmp/efatest/ && git pull $$EFA && cabal build && cabal haddock)

ghci:
	ghci -i:src:test -Wall -fwarn-incomplete-uni-patterns -fwarn-tabs demo/numericSolving/Main.hs

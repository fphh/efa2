SRCDIRS = src demo examples test tests

hlint.html: $(SRCDIRS) hlint/efa2.hs
	hlint $(SRCDIRS) --hint=hlint/efa2.hs --report=$@

ghci:
	ghci -i:src -Wall -fwarn-incomplete-uni-patterns -fwarn-tabs examples/elementary/newSolver/Main.hs

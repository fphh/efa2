SRCDIRS = src demo examples test tests

hlint.html: $(SRCDIRS) hlint/efa2.hs
	hlint $(SRCDIRS) --hint=hlint/efa2.hs --report=$@

hlint.html: src demo examples test tests
	hlint $^ --hint=hlint/efa2.hs --report=$@

.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	git ls-files '*.cabal' | xargs cabal-fmt --inplace


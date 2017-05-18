test:
	cabal configure --enable-tests
	cabal build
	cabal test --show-details='always'
.PHONY: test 

clean:
	cabal clean
.PHONY: clean 

test:
	cabal configure --enable-tests
	cabal build
	cabal test
.PHONY: test 

clean:
	cabal clean
.PHONY: clean 

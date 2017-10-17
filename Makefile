
build:
	cabal build

docs:
	cabal haddock

install: build docs
	cabal install --enable-documentation

clean:
	rm -rf dist/
	
uninstall: clean
	ghc-pkg unregister tx-events --force

reinstall: uninstall
	cabal install --force-reinstalls

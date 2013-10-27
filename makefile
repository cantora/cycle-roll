

dist/setup-config:
	cabal configure --enable-tests

.PHONY: build
build: dist/setup-config
	cabal build

.PHONY: tests
tests: build
	cabal test -v3

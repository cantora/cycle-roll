
SANDBOX_CONFIG = ./cabal.sandbox.config

.PHONY: install
install: dist/setup-config
	cabal install

$(SANDBOX_CONFIG):
	cabal sandbox init
	cabal install --force-reinstall --enable-tests --only-dependencies

dist/setup-config: $(SANDBOX_CONFIG)
	cabal configure --enable-tests

.PHONY: build
build: dist/setup-config
	cabal build

.PHONY: tests
tests: build
	cabal test -v3

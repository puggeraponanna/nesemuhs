run:
	cabal run

build:
	cabal build

test:
	cabal test --test-option=--color --test-show-details=always

setup:
	cabal install fourmolu stylish-haskell --overwrite-policy=always

fmt:
	@export PATH="$$PATH:$$HOME/.local/bin"; \
	which fourmolu > /dev/null || cabal install fourmolu --overwrite-policy=always; \
	which stylish-haskell > /dev/null || cabal install stylish-haskell --overwrite-policy=always; \
	./tools/format.py


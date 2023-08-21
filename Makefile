run:
	cabal run

build:
	cabal build

test:
	cabal test --test-option=--color --test-show-details=always

fmt:
	./tools/format.py

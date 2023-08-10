run:
	cabal run

build:
	cabal build

test:
	cabal test --test-show-details=always --test-option=--color

fmt:
	./tools/format.py

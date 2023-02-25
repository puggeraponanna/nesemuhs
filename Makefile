run:
	cabal run

build:
	cabal build

test:
	cabal test --test-show-details=always

fmt:
	./tools/format.py

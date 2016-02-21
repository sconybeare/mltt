.PHONY: all build clean configure haddock install repl run tags

shell = '$$SHELL'

all: install configure build haddock tags

build:
	cabal build --jobs

clean: nix-clean
	cabal clean
	-rm -f *.tix
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure

haddock: configure
	cabal haddock --hyperlink-source

install:
	cabal sandbox init
	cabal install --jobs --only-dependencies --reorder-goals

nix-clean:
	if test -e default.nix; then rm default.nix; fi
	if test -e shell.nix; then rm shell.nix; fi

nix-init: clean
	cabal2nix --shell . > shell.nix;
	cabal2nix . > default.nix;

nix-shell: nix-init
	nix-shell --command 'make install && $(shell)'
	make clean

repl:
	cabal repl lib:mltt

run:
	cabal run --jobs .

tags:
	hasktags -e .

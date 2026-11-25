#!/usr/bin/make

SHELL := /bin/bash

gatherflashes = ./test.el

emacs := /usr/bin/emacs

run:
	$(emacs) --script $(gatherflashes)

test:
	$(emacs) --script $(gatherflashes)

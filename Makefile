# Simple makefile. We make no particular effort to optimize dependencies,
# since compiling all files in a run is very fast anyway.

# This might not be needed on newer emacs versions, but it doesn't hurt.
VC_GIT_PATH=/usr/share/doc/git-core/contrib/emacs

EMACS_BATCH=emacs -Q --batch -L $(VC_GIT_PATH) -L .

.PHONY: all compile tags test clean

all: compile tags test

compile: *.el
	@echo "\n>>> Compiling"
	rm -f *.elc
	$(EMACS_BATCH) -f batch-byte-compile *.el

tags: *.el
	@echo "\n>>> Updating tags"
	etags *.el

test: *.el
	@echo "\n>>> Running tests"
	$(EMACS_BATCH) -l git-emacs.el -f git-regression

clean:
	rm -f *.elc

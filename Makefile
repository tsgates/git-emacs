# Simple makefile. We make no particular effort to optimize dependencies,
# since compiling all files in a run is very fast anyway.

# This might not be needed on newer emacs versions, but it doesn't hurt.
VC_GIT_PATH=/usr/share/doc/git-core/contrib/emacs

all: compile test

compile: *.el
	emacs --batch -L $(VC_GIT_PATH) -L . -f batch-byte-compile *.el

test: *.el
	emacs --batch -L $(VC_GIT_PATH) -L . -l git-emacs.el -f git-regression

clean:
	rm -f *.elc

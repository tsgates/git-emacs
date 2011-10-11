# Simple makefile. We make no particular effort to optimize dependencies,
# since compiling all files in a run is very fast anyway.

# This might not be needed on newer emacs versions, but it doesn't hurt.
# Note: newer git doesn't ship vc-git since it's been included in emacs,
# but that's only v23+. Oops. If that's your situation, add a path to
# your vc-git.el here (I store mine in .emacs.d now).
VC_GIT_PATH="-L /usr/share/doc/git-core/contrib/emacs -L ~/.emacs.d/"

EMACS_BATCH=emacs -Q --batch "$(VC_GIT_PATH)" -L .

.PHONY: all compile dev tags test clean

all: compile

dev: tags test

compile: *.el
	@echo; echo ">>> Compiling"
	rm -f *.elc
	$(EMACS_BATCH) -f batch-byte-compile *.el

tags: *.el
	@echo; echo ">>> Updating tags"
	etags *.el

test: *.el
	@echo; echo ">>> Running tests"
	$(EMACS_BATCH) -l git--test.el -f git-regression
	@echo; echo "Testing autoloads..."
	$(EMACS_BATCH) --eval "(require 'git-emacs-autoloads)" \
	  --visit "Makefile" \
	  --eval "(unless (functionp 'git-diff-baseline) (error \"autoload malfunctioned\"))"

clean:
	rm -f *.elc

all: test
.PHONY: all

.PHONY: info init check test test-ci test-guix devel clean clean-ci
info:
	eldev info
init:
	eldev init
check:
	eldev lint
test: clean
	eldev test -B
test-ci: clean-ci
	eldev -p prepare 2>/dev/null
	eldev -p -dtTC test -B
# below due to <https://github.com/emacs-eldev/eldev/issues/99>
test-guix:
	emacs --batch -l ox-tufte-test.el -f ert-run-tests-batch-and-exit
	emacs --batch -L . -f buttercup-run-discover
clean:
	eldev clean
clean-ci:
	rm -rf .eldev
devel: test
	{ \
	FILES="./*.el"; \
	DIRS=""; \
	EVENTS="modify,move,delete"; \
	EXCLUDE="\.#.*|.*~|.*\.swp|.*\.elc|flycheck.*"; \
	while inotifywait -qq $$FILES -e $$EVENTS -r $$DIRS --exclude $$EXCLUDE; do \
		make test; \
	done; \
	}

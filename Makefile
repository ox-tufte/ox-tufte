all: test
.PHONY: all

.PHONY: info init check test test-ci devel clean clean-ci
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

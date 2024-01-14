all: test
.PHONY: all

.PHONY: info init check test test-debug devel clean
info:
	eldev info
init:
	eldev init
check:
	eldev lint
test: clean
	eldev test -B
test-debug: clean
	eldev test
clean:
	eldev clean
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

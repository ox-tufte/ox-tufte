all: test
.PHONY: all

.PHONY: info build check clean dist distclean lint test devel
info:
	eldev info
build:
	eldev build
check:
	eldev -p prepare 2>/dev/null
	eldev -p -dtTC test -B
clean:
	eldev clean
dist:
	eldev package
distclean: clean
	rm -rf .eldev
	rm -rf dist
# NOTE: [[info:make#Standard Targets][make#Standard Targets]]
lint:
	eldev lint -f ox-tufte.el
test: clean
	eldev -dtTC test -B
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

.PHONY: guix guix-profile guix-profile-update
channels := ./.guix/channels.scm
manifest := ./.guix/manifest.scm
guix:
	guix build -K -f guix.scm
guix-profile:
	@mkdir -p ~/.guix-extra-profiles/ox-tufte
	guix time-machine -C $(channels) -- \
		package --fallback \
		-p ~/.guix-extra-profiles/ox-tufte/ox-tufte \
		-m $(manifest)
guix-profile-update:
	./.guix/etc/update-channels.scm > $(channels)
	$(MAKE) guix-profile

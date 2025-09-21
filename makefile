.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format
format:
	bunx elm-format --yes src/ tests/


.PHONY: test
test:
	bunx elm-test


.PHONY: build
build: test docs.json


.PHONY: dev
dev:
	bunx elm reactor


docs.json: node_modules src elm.json
	bunx elm make --docs=$@


node_modules: package.json bun.lock
	if test ! -d $@; \
	then npm install; \
	fi

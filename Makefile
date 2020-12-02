DAYS := $(wildcard */.)

all: ${DAYS}

${DAYS}:
	${MAKE} -C $@

.PHONY: all ${DAYS}

day_%: .template
	cp -nr $< $@
	git add $@

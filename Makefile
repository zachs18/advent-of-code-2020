DAYS := $(wildcard */.)

all: ${DAYS}

${DAYS}:
	${MAKE} -C $@

.PHONY: all ${DAYS}

day_%: .template
	[ -e $@ ] || cp -nr $< $@
	git add $@

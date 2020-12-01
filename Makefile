DAYS := $(wildcard */.)

all: ${DAYS}

${DAYS}:
	${MAKE} -C $@

.PHONY: all ${DAYS}

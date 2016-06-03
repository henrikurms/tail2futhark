include ../common.mk

APLFILES=$(wildcard *.apl)
FUTEXECS=$(APLFILES:%.apl=fut_%)
TLEXECS=$(APLFILES:%.apl=tail_%)

.PHONY: all
all: $(FUTEXECS) $(TLEXECS)

clean:
	make -C life clean
	rm -f *~ *.fut *.tail tail_* fut_*

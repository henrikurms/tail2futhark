T2F=~/tail2futhark/dist/build/tail2futhark/tail2futhark
FUTHARKC=~/futhark/dist/build/futhark-c/futhark-c
APLT_ROOT=~/apltail
APLT=$(APLT_ROOT)/aplt
PRELUDE=$(APLT_ROOT)/lib/prelude.apl
INCLUDE=$(APLT_ROOT)/include

APLFILES=$(wildcard *.apl)
FUTEXECS=$(APLFILES:%.apl=fut_%)
TLEXECS=$(APLFILES:%.apl=tail_%)

.PHONY: all
all: $(FUTEXECS) $(TLEXECS)

tail_%.c: %.apl
	$(APLT) -silent -c -oc $@ $(PRELUDE) $<

tail_%: tail_%.c
	cc -lm -std=c99 -o $@ -I $(INCLUDE) $<

%.tail: %.apl
	$(APLT) -p_types -p_tail -silent -c -o $@ $(PRELUDE) $< 

%.fut: %.tail
	$(T2F) -o $@ $< 

fut_%: %.fut
	$(FUTHARKC) -o $@ $<

clean:
	rm *.fut *.tail tail_* fut_*

T2F=~/gits/tail2futhark/dist/build/tail2futhark/tail2futhark
FUTHARKC=futhark-c
APLT_ROOT=~/gits/apltail
APLT=$(APLT_ROOT)/aplt
PRELUDE=$(APLT_ROOT)/lib/prelude.apl
INCLUDE=$(APLT_ROOT)/include

APLFILES=$(wildcard *.apl)
FUTEXECS=$(APLFILES:%.apl=fut_%)
TLEXECS=$(APLFILES:%.apl=tail_%)

.PHONY: all
all: $(FUTEXECS) $(TLEXECS)

tail_%.c: %.apl
	$(APLT) -silent -c -O2 -opt_hoist -oc $@ $(PRELUDE) $<

tail_%: tail_%.c
	gcc -lm -std=c99 -O3 -o $@ -I $(INCLUDE) $<

%.tail: %.apl
	$(APLT) -p_types -p_tail -silent -c -o $@ $(PRELUDE) $< 

%.fut: %.tail
	$(T2F) -o $@ $< 

fut_%: %.fut
	$(FUTHARKC) -o $@ $<

clean:
	rm -f *.fut *.tail tail_* fut_*

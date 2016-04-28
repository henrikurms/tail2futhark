T2F=~/.local/bin/tail2futhark
FUTHARKC=futhark-c
APLT_ROOT=~/gits/apltail
APLT=$(APLT_ROOT)/aplt
PRELUDE=$(APLT_ROOT)/lib/prelude.apl
INCLUDE=$(APLT_ROOT)/include

APLFILES=$(wildcard *.apl)
FUTEXECS=$(APLFILES:%.apl=fut_%)
TLEXECS=$(APLFILES:%.apl=tail_%)
OUTFILES=$(APLFILES:%.apl=%.out)
RESFILES=$(APLFILES:%.apl=%.res)
TAILFILES=$(APLFILES:%.apl=%.tail)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"

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

%.out: fut_%
	./$< > $@

%.res: %.out
	@(diff -aq $< $*.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.apl: OK" > $@ \
         ; else \
             if [ -e $*.ok ]; then \
                echo "Test $*.apl: *** ERR: file $< differs from $*.ok ***" > $@ \
             ; else \
                echo "Test $*.apl: *** ERR: file $*.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: clean
clean:
	rm -f *.fut tail_* fut_* *.res *.out *.comp

.PHONY: realclean
realclean:
	make clean
	rm -f *.tail

include ../common.mk

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

include ../common.mk

APLFILES=$(wildcard *.apl)
FUTEXECS=$(APLFILES:%.apl=fut_%)
TLEXECS=$(APLFILES:%.apl=tail_%)
OUTFILES=$(APLFILES:%.apl=%.out)
RESFILES=$(APLFILES:%.apl=%.res)
TAILFILES=$(APLFILES:%.apl=%.tail)
RESOPENCLFILES=$(APLFILES:%.apl=%.resopencl)

OPENCL_DEVICE?=GeForce

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

.PHONY: testopencl
testopencl: $(RESOPENCLFILES)
	@cat $(RESOPENCLFILES)
	@echo "-------O p e n C L --- T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESOPENCLFILES) | wc -l` /`grep "Test" $(RESOPENCLFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESOPENCLFILES) | wc -l` /`grep "Test" $(RESOPENCLFILES) | wc -l`"
	@echo "-----------------------------------------------------"
	@exit `grep "ERR" $(RESOPENCLFILES) | wc -l`

%.out: fut_%
	./$< > $@

%.outopencl: futopencl_%
	./$< -d $(OPENCL_DEVICE) > $@

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

%.resopencl: %.outopencl
	@(diff -aq $< $*.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "OpenCL Test $*.apl: OK" > $@ \
         ; else \
             if [ -e $*.ok ]; then \
                echo "OpenCL Test $*.apl: *** ERR: file $< differs from $*.ok ***" > $@ \
             ; else \
                echo "OpenCL Test $*.apl: *** ERR: file $*.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: clean
clean:
	rm -f *.fut tail_* fut_* *.res *.out *.comp futopencl_* *.outopencl *.resopencl *~ *.tail

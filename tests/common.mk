# Standard paths and definitions used by all the Makefiles in here.

T2F       ?= tail2futhark
FUTHARKC  ?= futhark-c
FUTHARKOPENCL ?= futhark-opencl
APLT_ROOT ?= ~/gits/apltail
APLT      ?= $(APLT_ROOT)/aplt
PRELUDE   ?= $(APLT_ROOT)/lib/prelude.apl
INCLUDE   ?= $(APLT_ROOT)/include

tail_%.c: %.apl
	$(APLT) -silent -c -O2 -opt_hoist -oc $@ $(PRELUDE) $<

tail_%: tail_%.c
	gcc -lm -std=c99 -O3 -o $@ -I $(INCLUDE) $<

%.tail: %.apl
	$(APLT) -p_types -p_tail -s_tail -silent -c -o $@ $(PRELUDE) $< 

%.fut: %.tail
	$(T2F) -o $@ $< 

fut_%: %.fut
	$(FUTHARKC) -o $@ $<

futopencl_%: %.fut
	$(FUTHARKOPENCL) -o $@ $<

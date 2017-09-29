# Standard paths and definitions used by all the Makefiles in here.

ifndef TAIL_ROOT
$(error TAIL_ROOT is not set)
endif

.SECONDARY:

T2F       ?= tail2futhark
FUTHARKC  ?= futhark-c
FUTHARKOPENCL ?= futhark-opencl
APLT      ?= $(TAIL_ROOT)/aplt
PRELUDE   ?= $(TAIL_ROOT)/lib/prelude.apl
INCLUDE   ?= $(TAIL_ROOT)/include

tail_%.c: %.apl
	$(APLT) -silent -c -O2 -opt_hoist -oc $@ $(PRELUDE) $<

tail_%: tail_%.c
	gcc -lm -std=c99 -O3 -o $@ -I $(INCLUDE) $<

%.tail: %.apl
	$(APLT) -p_types -p_tail -s_tail -silent -c -o $@ $(PRELUDE) $< 

%.fut: %.tail
	$(T2F) -o $@ $< 

%.futu: %.tail
	$(T2F) --unsafe -o $@ $< 

fut_%: %.fut
	$(FUTHARKC) -o $@ $<

futopencl_%: %.futu
	$(FUTHARKOPENCL) -o $@ $<

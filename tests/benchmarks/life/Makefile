EXAMPLES=ex.rle gosper.rle 3enginecordershiprake.rle
FUTINS=$(EXAMPLES:%.rle=%.futin)
FUTOUTS=$(EXAMPLES:%.rle=%.futout)
FUTSEES=$(EXAMPLES:%.rle=%.futsee)

all: rle $(FUTINS) $(FUTOUTS)

rle: rle.sml
	mlkit -o rle rle.sml

clean:
	rm -rf MLB *~ rle *.futin *.futout *.futsee


%.futin: %.rle rle
	./rle --flat < $< > $@

%.futout: %.futin
	../fut_lifein < $< >$@

%.futsee: %.futout Makefile
	sed 's/111i32/o/g' < $< | \
	sed 's/32i32/_/g' | \
	sed 's/\[//g' | \
	tr ']' '\n' | \
	sed 's/, //g' > $@

include ../../common.mk

withaplt:
	$(APLT) $(PRELUDE) ../lifein.apl

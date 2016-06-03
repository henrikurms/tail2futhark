EXAMPLES=ex.rle gosper.rle 3enginecordershiprake.rle
FUTINS=$(EXAMPLES:%.rle=%.futin)
FUTOUTS=$(EXAMPLES:%.rle=%.futout)

all: rle $(FUTINS) $(FUTOUTS)

rle: rle.sml
	mlkit -o rle rle.sml

clean:
	rm -rf MLB *~ rle *.futin *.futout


%.futin: %.rle rle
	./rle --flat < $< > $@

%.futout: %.futin
	../fut_lifein < $< $@

include ../../common.mk

withaplt:
	$(APLT) $(PRELUDE) ../lifein.apl

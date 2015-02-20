
APLACC = ../../dist/build/aplacc/aplacc
APLACCFLAGS =

GHC = ghc
GHCFLAGS = -i../../src

TEST_FILES = $(wildcard *.tail)

clean:
	rm -f $(basename $(TEST_FILES))
	rm -f ../../src/APLAcc/Primitives.hi ../../src/APLAcc/Primitives.o

%-cuda: APLACCFLAGS += --cuda
%-cuda: GHCFLAGS += -threaded
%-cuda: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
	rm $*.hi $*.o

%: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<
	rm $*.hi $*.o

%.hs: %.tail
	$(APLACC) $(APLACCFLAGS) $< > $@

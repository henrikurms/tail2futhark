
.PHONY: all
all:
	stack build

.PHONY: install
install:
	stack install

.PHONY: test
test:
	make -C tests/basic_tests test

.PHONY: clean
clean:
	make -C tests/basic_tests clean

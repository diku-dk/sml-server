MLCOMP ?= mlkit

.PHONY: all
all:
	$(MLCOMP) -output server.exe server.mlb

.PHONY: test
test:
	$(MAKE) -C test test

.PHONY: clean
clean:
	$(MAKE) -C test clean
	rm -rf MLB *~ server.exe

MLCOMP ?= mlkit

.PHONY: all
all:
	$(MLCOMP) -output http.exe http.mlb

.PHONY: test
test:
	$(MAKE) -C test test

.PHONY: clean
clean:
	$(MAKE) -C test clean
	rm -rf MLB *~ http.exe

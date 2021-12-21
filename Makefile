SMLPKG ?= smlpkg

.PHONY: all
all: lib/github.com/diku-dk/sml-http
	$(MAKE) -C lib/github.com/diku-dk/sml-server all

.PHONY: test
test: lib/github.com/diku-dk/sml-http
	$(MAKE) -C lib/github.com/diku-dk/sml-server test

.PHONY: clean
clean:
	($(MAKE) -C lib/github.com/diku-dk/sml-http clean; exit 0)
	($(MAKE) -C lib/github.com/diku-dk/sml-parse clean; exit 0)
	$(MAKE) -C lib/github.com/diku-dk/sml-server clean
	find . -name 'MLB' | xargs rm -rf
	rm -rf MLB *~ .*~

.PHONY: realclean
realclean:
	$(MAKE) clean
	rm -rf lib/github.com/diku-dk/sml-parse
	rm -rf lib/github.com/diku-dk/sml-http

lib/github.com/diku-dk/sml-http:
	$(SMLPKG) sync

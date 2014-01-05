CODENAME = ""
XDG ?= \#f
SED_XDG = s,@@use-xdg-paths@@,$(XDG),
SED_CN = s,@@prg-code-name@@,$(CODENAME),

.SUFFIXES:
.SUFFIXES: .1 .1.t2t

all: olscrbl/build-configuration.scm bin/olscrbl

test: unit-tests functional-tests

functional-tests:
	@printf '\n Running functional test-suite:\n\n'
	@cd t && sh all.sh
	@printf '\n'

unit-tests:
	@sh run-unit-tests.sh

doc: olscrbl.1

happiness: all doc test

clean:
	rm -f olscrbl/build-configuration.scm bin/olscrbl
	rm -f olscrbl.1 olscrbl.1.t2t
	rmdir bin || true
	find . -name "*.go" -exec rm -f "{}" +
	rm -Rf t/twd
	rm -Rf t/logs

olscrbl/build-configuration.scm: olscrbl-build-configuration.in
	sed -e "$(SED_XDG)" -e "$(SED_CN)" < $< > $@

bin/olscrbl: olscrbl.in
	test -d bin || mkdir bin
	sed -e "$(SED_XDG)" -e "$(SED_CN)" < $< > $@
	test -x $@ || chmod +x $@

.1.t2t.1:
	txt2tags --target man -o- $< | sed -e '/^$$/d' -e 's/^\\e$$//' > $@

olscrbl.1.t2t: olscrbl.1.t2t.in
	perl -npe 'if ("$(XDG)" eq "#t") { s,\@\@USERDIR\@\@,\$${XDG_CONFIG_HOME},g; } else { s,\@\@USERDIR\@\@,~/.olscrbl,g; }' < $< > $@

.PHONY: all doc clean test happiness functional-tests unit-tests

GZIP=gzip -f
ASCIIDOC=asciidoctor -b manpage

MANPAGES=serlo2adoc.1.gz

.PHONY: man
man: $(MANPAGES)

%.gz : %
	$(GZIP) $<

%.1: %.1.adoc
	$(ASCIIDOC) -b manpage -d manpage $<

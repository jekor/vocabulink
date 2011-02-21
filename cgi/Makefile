# Vocabulink CGI

cgi := vocabulink.cgi
hss := Vocabulink.hs $(shell find Vocabulink -regex ".*l?hs" -print)
date := $(shell date +%Y-%m-%d)
binary := dist/build/$(cgi)/$(cgi)
DIST := vocabulink--$(date)

all : $(binary)

doc : vocabulink.pdf

configure : $(hss)
	cabal configure

$(binary) : $(hss) ../hamlet/*.hamlet
	TPG_DB="vocabulink" TPG_USER="vocabulink" cabal build

vocabulink.pdf : vocabulink.tex
	pdflatex $<

vocabulink.tex : $(hss)
	lhs2TeX Vocabulink.lhs > $@

install : $(binary)
	mv ../$(cgi) ../$(cgi).old
	cp $(binary) ../$(cgi)
	strip ../$(cgi)

tags : $(hss)
	hasktags -e $^

lint : $(hss)
	hlint $^

clean :
	cabal clean

metrics : $(hss)
	@echo -e -n "\tSLOC: "
	@lhs2TeX --code Vocabulink.lhs | wc --lines | tr -d "\n"
	@echo -e -n "\tgzipped bytes: "
	@lhs2TeX --code Vocabulink.lhs | gzip | wc --bytes | tr -d "\n"
	@echo -e -n "\tsource chars: "
	@lhs2TeX --code Vocabulink.lhs | wc --chars | tr -d "\n"
	@echo -e -n "\t  total chars: "
	@cat $^ | wc --chars | tr -d "\n"
	@echo
	@echo
	@ls -l $^

dist : $(DIST).tar.gz

$(DIST).tar.gz :
#	`darcs dist` would be nice, but the darcs repository is the parent directory
#	of this one and needs to be cleaned up a bit. For now, we'll have to do it
#	ourselves.
	echo README COPYING Makefile vocabulink.cabal ../vocabulink.sql ../js/{comment,form,link-editor,review,star-rating}.js ../js/Makefile ../css/*.sass ../css/Makefile ../etc/vocabulink.conf.example ../tests/{Makefile,web.lhs} `find -name "*.lhs" -o -name "*.lhs-boot"` | xargs tar --transform 's,^(\./)?,$(DIST)/,x' -czvf $(DIST).tar.gz

publish : $(DIST).tar.gz
	cp $(DIST).tar.gz ~/project/jekor.com/vocabulink/
	rm -f ~/project/jekor.com/vocabulink/vocabulink.tar.gz
	pushd ~/project/jekor.com/vocabulink && ln -s $(DIST).tar.gz vocabulink.tar.gz
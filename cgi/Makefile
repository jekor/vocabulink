# Vocabulink CGI

cgi := vocabulink.cgi
lhss := $(shell find . -name "*.lhs" -print)
date := $(shell date +%Y-%m-%d)
DIST := vocabulink--$(date)

all : $(cgi)

doc : vocabulink.pdf

vocabulink.cgi : $(lhss)
	runhaskell Setup.lhs build

vocabulink.pdf : vocabulink.tex
	pdflatex $<

vocabulink.tex : $(lhss)
	lhs2TeX Vocabulink.lhs > $@

install : dist/build/$(cgi)/$(cgi)
	cp ~/.cabal/bin/vocabulink.cgi ~/.cabal/bin/vocabulink.cgi.backup
	runhaskell Setup.lhs install
	-killall $(cgi)

tags : $(lhss)
	hasktags -e $^

lint : $(lhss)
	hlint $^

clean :
	find . -iregex ".*\.\(o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

metrics : $(lhss)
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

$(DIST).tar.gz: vocabulink.pdf
#	`darcs dist` would be nice, but the darcs repository is the parent directory
#	of this one and needs to be cleaned up a bit. For now, we'll have to do it
#	ourselves.
	echo README COPYING Makefile vocabulink.cabal vocabulink.pdf ../vocabulink.sql ../js/{comment,form,link-graph,link-editor,review,star-rating}.js ../js/Makefile ../js/{raphael,functional,jquery.markitup,markdown.set,showdown,ajaxupload}.js ../css/*.ccss ../css/Makefile ../etc/vocabulink.conf.example ../xhtml-unicode.patch ../tests/{Makefile,web.lhs,web.pdf} `find -name "*.lhs" -o -name "*.lhs-boot"` | xargs tar --transform 's,^(\./)?,$(DIST)/,x' -czvf $(DIST).tar.gz

publish : $(DIST).tar.gz
	cp $(DIST).tar.gz vocabulink.pdf ~/project/jekor.com/vocabulink/
	rm -f ~/project/jekor.com/vocabulink/vocabulink.tar.gz
	pushd ~/project/jekor.com/vocabulink && ln -s $(DIST).tar.gz vocabulink.tar.gz
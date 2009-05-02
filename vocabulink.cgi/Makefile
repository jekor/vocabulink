# Vocabulink CGI

fcgi := vocabulink.fcgi
lhss := $(shell find . -name "*.lhs" -print)
date := $(shell date +%Y-%m-%d)
DIST := vocabulink--$(date)

all : $(fcgi)

doc : vocabulink.pdf

vocabulink.fcgi : $(lhss)
	ghc -Wall -fglasgow-exts -threaded -package fastcgi --make -o $(fcgi) $^

vocabulink.pdf : Vocabulink.tex
	pdflatex $<

Vocabulink.tex : $(lhss)
	lhs2TeX Vocabulink.lhs > $@

install : vocabulink.fcgi
	install -o root -g root $(fcgi) /usr/local/bin
# The following is now handled by an init script.
#	-killall $(fcgi)
#	spawn-fcgi -f "/usr/local/bin/$(fcgi) 2>> /tmp/vocabulink.error.log" -p 10033 -u lighttpd -g lighttpd

tags : $(lhss)
	hasktags -e $^

clean :
	find . -iregex ".*\.\(o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

test :
	ghci -fglasgow-exts Vocabulink.lhs

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

$(DIST).tar.gz: Vocabulink.pdf
#	`darcs dist` would be nice, but the darcs repository is the parent directory
#	of this one and needs to be cleaned up a bit. For now, we'll have to do it
#	ourselves.
	echo README COPYING Makefile ../vocabulink.sql ../js/{form,forum,forum-comment,link,link-graph,page,review}.js ../css/*.ccss ../css/Makefile `find -name "*.lhs" -o -name "*.lhs-boot"` | xargs tar --transform 's,^(\./)?,$(DIST)/,x' -czvf $(DIST).tar.gz

publish : $(DIST).tar.gz
	cp $(DIST).tar.gz Vocabulink.pdf ~/project/jekor.com/vocabulink/
	rm -f ~/project/jekor.com/vocabulink/vocabulink.tar.gz
	pushd ~/project/jekor.com/vocabulink && ln -s $(DIST).tar.gz vocabulink.tar.gz
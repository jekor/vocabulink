# Vocabulink CGI

fcgi := vocabulink.fcgi

lhss := $(shell find . -name "*.lhs" -print)

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
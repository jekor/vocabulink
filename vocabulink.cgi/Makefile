# Vocabulink CGI

fcgi := vocabulink.fcgi

lhss := $(shell find . -name "*.lhs" -print)

all : $(fcgi)

vocabulink.fcgi : $(lhss)
	ghc -Wall -fglasgow-exts -threaded -package fastcgi --make -o $(fcgi) $^

install :
	install -o root -g root $(fcgi) /usr/local/bin
	-killall $(fcgi)
	spawn-fcgi -f "/usr/local/bin/$(fcgi) 2>> /tmp/vocabulink.error.log" -p 10033 -u lighttpd -g lighttpd

tags : $(lhss)
	hasktags -e $^

clean :
	find . -name "*.o" -o -name "*.hi" -print0 | xargs -0 rm

test :
	ghci -fglasgow-exts Vocabulink.lhs

metrics : $(lhss) $(shell find . -name "*.lhs-boot" -print)
	@echo -e -n "\tSLOC: "
	@echo $^ | xargs -n 1 lhs2TeX --code | wc --lines | tr -d "\n"
	@echo -e -n "\tgzipped bytes: "
	@echo $^ | xargs -n 1 lhs2TeX --code | gzip | wc --bytes | tr -d "\n"
	@echo -e -n "\tsource chars: "
	@echo $^ | xargs -n 1 lhs2TeX --code | wc --chars | tr -d "\n"
	@echo -e -n "\t  total chars: "
	@cat $^ | wc --chars | tr -d "\n"
	@echo
	@echo
	@ls -l $^
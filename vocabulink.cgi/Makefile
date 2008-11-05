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

clean:
	find . -name "*.o" -o -name "*.hi" -print0 | xargs -0 rm

test :
	ghci -fglasgow-exts Vocabulink.lhs

sloc : $(lhss)
	echo $^ | xargs -n 1 lhs2TeX --code | wc --lines

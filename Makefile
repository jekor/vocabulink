# Vocabulink CGI

FCGI = vocabulink.fcgi

all : $(FCGI)

vocabulink.fcgi : Vocabulink.lhs Vocabulink/*lhs
	ghc -Wall -threaded -package fastcgi --make -o $(FCGI) $^

install :
	install -o root -g root $(FCGI) /usr/local/bin
	-killall $(FCGI)
	spawn-fcgi -f "/usr/local/bin/$(FCGI) 2>> /tmp/vocabulink.error.log" -p 10033 -u lighttpd -g lighttpd
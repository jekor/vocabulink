# Vocabulink flashcard CGI

all : flashcard.fcgi

flashcard.fcgi : *.lhs Flashcard/*lhs
	ghc -Wall -threaded -package fastcgi --make -o flashcard.fcgi $?

install :
	install -o root -g root flashcard.fcgi /usr/local/bin
	-killall flashcard.fcgi
	spawn-fcgi -f "/usr/local/bin/flashcard.fcgi 2>> /tmp/flashcard.error.log" -p 10033 -u lighttpd -g lighttpd
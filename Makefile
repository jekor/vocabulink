# Vocabulink flashcard CGI

all : flashcard.fcgi

flashcard.fcgi : flashcard.lhs
	ghc -Wall -threaded -package fastcgi --make -o flashcard.fcgi flashcard.lhs

install :
	install -o root -g root flashcard.fcgi /usr/local/bin
	/etc/init.d/spawn-fcgi restart
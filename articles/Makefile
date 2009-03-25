# Vocabulink articles

sources := $(shell ls *.muse)
articles := $(sources:.muse=.html)

all : $(articles)

%.html : %.muse
	emacs -q -batch -l muse-init.el -f muse-batch-publish-files xhtml $<

clean :
	rm $(articles)

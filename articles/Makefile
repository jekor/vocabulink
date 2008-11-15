# Vocabulink articles

sources := *.muse
articles := $(sources:.muse=.html)

all : $(articles)

$(articles) : $(sources)
	emacs -q -batch -l muse-init.el -f muse-batch-publish-files xhtml $^

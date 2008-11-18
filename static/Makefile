# Vocabulink articles

sources := *.muse
statics := $(sources:.muse=.html)

all : $(statics)

$(statics) : $(sources)
	emacs -q -batch -l muse-init.el -f muse-batch-publish-files xhtml $^

# Vocabulink CSS (SASS) files
# See http://sass-lang.com/

sources := $(shell ls *.sass | grep -v lib.sass)
csses := $(sources:.sass=.css)

all : $(csses) lib.common.css lib.member.css

lib.common.css : common.css comment.css
	cat $^ > $@

lib.member.css : link-editor.css markitup-set.css markitup-skin.css jquery-loadmask.css
	cat $^ > $@

%.css : %.sass lib.sass
	sass $< > $@

clean :
	rm $(csses) lib.common.css lib.member.css
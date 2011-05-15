# Vocabulink CSS (SASS) files
# See http://sass-lang.com/

sources := $(shell ls *.sass | grep -v lib.sass)
csses := $(sources:.sass=.css)
libs := lib.common.css lib.member.css

all : $(csses) $(libs)

lib.common.css : common.css comment.css jquery.toastmessage.css
	cat $^ > $@

lib.member.css : link-editor.css markitup-set.css markitup-skin.css jquery-loadmask.css fileuploader.css
	cat $^ > $@

%.css : %.sass lib.sass
	sass $< > $@

clean :
	rm $(csses) $(libs)
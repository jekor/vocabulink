# Vocabulink (Clever) CSS files
# See http://sandbox.pocoo.org/clevercss-hs/

sources := $(shell ls *.ccss)
csses := $(sources:.ccss=.css)
clevercss := clevercss

all : $(csses) lib.common.css lib.member.css
	cp *.css ../s/css/

lib.common.css : common.css page.css comment.css
	cat $^ > $@

lib.member.css : link-editor.css markitup-set.css markitup-skin.css
	cat $^ > $@

%.css : %.ccss constants
	cat constants $< | $(clevercss) > $@

clean :
	rm $(csses)
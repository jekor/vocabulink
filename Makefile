# Vocabulink site

cgi := vocabulink.cgi
hses := cgi/Vocabulink.hs $(shell find cgi/Vocabulink -name "*.hs")
jses := $(shell find js -maxdepth 1 -name "*.js")
minjses := js/compiled/common.js js/compiled/link.js js/compiled/member.js js/compiled/raphael.js js/compiled/metrics.js js/compiled/review.js js/compiled/dashboard.js
sasses := $(shell find css -maxdepth 1 -name "*.sass" | grep -v lib.sass)
csses := $(sasses:.sass=.css)
csslibs := css/compiled/common.css css/compiled/link.css css/compiled/member.css css/compiled/metrics.css css/compiled/article.css css/compiled/dashboard.css css/compiled/member-page.css css/compiled/review.css css/compiled/front.css
markdowns := $(shell find -name "*.markdown")
articles := $(markdowns:.markdown=.html)
chapters:= $(shell ls handbook/chapters/*.tex)

sync_options := -avz --exclude 'cgi/dist' --exclude '*.sass' --exclude '.sass-cache' --exclude '*.aux' --exclude '*.tex' --exclude '*.ptb' --exclude '*.log' --exclude '*.out' --exclude '._*' --exclude '.DS_Store' --delete articles audio css etc img js s scripts offline vocabulink.cgi vocabulink.com:vocabulink/

all : $(cgi) css js articles handbook offline

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

css : $(csslibs)

%.css : %.sass css/lib.sass
	sass $< > $@

css/compiled/common.css : css/common.css css/comment.css css/external/jquery.toastmessage.css css/external/jquery-loadmask.css css/external/jquery.simplemodal.css
	cat $^ > $@

css/compiled/link.css : css/link.css
	cat $^ > $@

css/compiled/member.css : css/link-editor.css css/external/markitup-set.css css/external/markitup-skin.css
	cat $^ > $@

css/compiled/metrics.css : css/metrics.css
	cat $^ > $@

css/compiled/article.css : css/article.css
	cat $^ > $@

css/compiled/dashboard.css : css/dashboard.css
	cat $^ > $@

css/compiled/member-page.css : css/member-page.css
	cat $^ > $@

css/compiled/review.css : css/review.css
	cat $^ > $@

css/compiled/front.css : css/front.css
	cat $^ > $@

js : $(minjses)

# This is getting large. I'd like to break it up and do deferred loading at some point.
js/compiled/common.js : js/external/jquery-1.6.1.js js/external/jquery.cookie.js js/external/minform.js js/external/jquery.loadmask.js js/external/jquery.toastmessage.js js/common.js js/external/jquery.simplemodal-1.4.1.js
	cat $^ | jsmin > $@

# link viewing, not editing
js/compiled/link.js : js/link.js js/external/longtable.js
	cat $^ | jsmin > $@

# This is for email-verified members.
js/compiled/member.js : js/external/jquery.markitup.js js/external/markdown.set.js js/external/showdown.js js/ajax.js js/external/chartviz.js js/comment.js js/link-editor.js
	cat $^ | jsmin > $@

js/compiled/review.js : js/review.js js/external/jquery.hotkeys.js
	cat $^ | jsmin > $@

js/compiled/raphael.js : js/external/raphael.js
	cat $^ | jsmin > $@

js/compiled/metrics.js : js/metrics.js
	cat $^ | jsmin > $@

js/compiled/dashboard.js : js/external/drcal.js js/dashboard.js
	cat $^ | jsmin > $@

# TODO: Switch these to git submodules?
js/external/chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@

js/external/minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

js/external/longtable.js : /home/jekor/project/longtable/longtable.js
	cp $^ $@

js/external/drcal.js : /home/jekor/project/drcal/drcal.js
	cp $^ $@

# TODO: Add command to fetch raphael, jquery, jquery plugins, showdown, etc. into external/

handbook : handbook/handbook.pdf

handbook/handbook.pdf : handbook/handbook.tex $(chapters)
	cd handbook && xelatex handbook

articles : $(articles)

%.html : %.markdown articles/template.html
	pandoc --smart --section-divs --mathjax -t html5 --toc --standalone --template=articles/template.html < $< > $@

cgi/dist/setup-config : cgi/vocabulink.cabal
	cd cgi && cabal configure

cgi/dist/build/$(cgi)/$(cgi) : cgi/dist/setup-config $(hses)
	cd cgi && TPG_DB="vocabulink" TPG_USER="vocabulink" cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)

$(cgi) : cgi/dist/build/$(cgi)/$(cgi)
	mv $(cgi) $(cgi).old
	cp $^ $@
	strip $@

# TODO: Make lint rules so that the appropriate lint is run automatically when the file is changed (before proceeding to the compilation step).

hlint : $(hses)
	hlint -i "Redundant do" $^

jslint : $(jses)
	cat $^ | jslint

offline : offline/lib.offline.js offline/offline.css

offline/lib.offline.js : offline/jquery-1.7.1.js offline/offline.js
	cat $^ | jsmin > $@

clean :
	rm handbook/*.aux handbook/*.ilg handbook/*.log handbook/*.out handbook/*.toc handbook/chapters/*.aux

cloc :
	cloc $(hses) $(jses) $(csses)
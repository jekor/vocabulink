# Vocabulink site

cgi := vocabulink.cgi
hses := cgi/Vocabulink.hs $(shell find cgi/Vocabulink -name "*.hs")
jses := $(shell find js -maxdepth 1 -name "*.js")
minjses := js/compiled/common.js js/compiled/link.js js/compiled/member.js js/compiled/review.js js/compiled/dashboard.js
sasses := $(shell find css -maxdepth 1 -name "*.sass" | grep -v lib.sass)
csses := css/compiled/common.css css/compiled/link.css css/compiled/member.css css/compiled/article.css css/compiled/dashboard.css css/compiled/member-page.css css/compiled/review.css css/compiled/front.css
markdowns := $(shell find -name "*.markdown")
articles := $(markdowns:.markdown=.html)
chapters:= $(shell ls handbook/chapters/*.tex)

sync_options := -avz --exclude 'cgi/dist' --exclude '*.sass' --exclude '.sass-cache' --exclude '*.aux' --exclude '*.tex' --exclude '*.ptb' --exclude '*.log' --exclude '*.out' --exclude '._*' --exclude '.DS_Store' --delete articles audio css etc img js s scripts offline vocabulink.cgi vocabulink.com:vocabulink/

all : $(cgi) js css articles handbook offline

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

js : $(minjses)

# This is getting large. I'd like to break it up and do deferred loading at some point.
js/compiled/common.js : js/external/jquery-1.6.1.js js/external/jquery.cookie.js js/external/minform.js js/external/jquery.loadmask.js js/external/jquery.toastmessage.js js/external/jquery.simplemodal-1.4.1.js js/common.js
	cat $^ | jsmin > $@

# link viewing, not editing
js/compiled/link.js : js/external/longtable.js js/link.js
	cat $^ | jsmin > $@

# This is for email-verified members.
js/compiled/member.js : js/external/jquery.markitup.js js/external/markdown.set.js js/external/showdown.js js/ajax.js js/comment.js js/link-editor.js
	cat $^ | jsmin > $@

js/compiled/review.js : js/external/jquery.hotkeys.js js/review.js
	cat $^ | jsmin > $@

js/compiled/dashboard.js : js/external/drcal.js js/dashboard.js
	cat $^ | jsmin > $@

js/external/minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

js/external/longtable.js : /home/jekor/project/longtable/longtable.js
	cp $^ $@

js/external/drcal.js : /home/jekor/project/drcal/drcal.js
	cp $^ $@

css : $(csslibs)

css/compiled/common.css : css/lib.sass css/common.sass css/comment.sass css/external/jquery.toastmessage.sass css/external/jquery-loadmask.sass css/external/jquery.simplemodal.sass
	cat $^ | sass > $@

css/compiled/link.css : css/lib.sass css/link.sass
	cat $^ | sass > $@

css/compiled/member.css : css/lib.sass css/link-editor.sass css/external/markitup-set.sass css/external/markitup-skin.sass
	cat $^ | sass > $@

css/compiled/article.css : css/lib.sass css/article.sass
	cat $^ | sass > $@

css/compiled/dashboard.css : css/lib.sass css/dashboard.sass
	cat $^ | sass > $@

css/compiled/member-page.css : css/lib.sass css/member-page.sass
	cat $^ | sass > $@

css/compiled/review.css : css/lib.sass css/review.sass
	cat $^ | sass > $@

css/compiled/front.css : css/lib.sass css/front.sass
	cat $^ | sass > $@

# TODO: Add command to fetch jquery, jquery plugins, showdown, etc. into external/

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

hlint : $(hses)
	hlint -i "Redundant do" -i "Use camelCase" $^

# For jslint, go to http://www.jslint.com/
# /*jslint browser: true, devel: true, nomen: true, plusplus: true, regexp: true, sloppy: true, vars: true, white: true, indent: 2 */
# jslint : $(jses)
#	cat $^ | jslint

offline : offline/lib.offline.js offline/offline.css

offline/lib.offline.js : offline/jquery-1.7.1.js offline/offline.js
	cat $^ | jsmin > $@

clean :
	rm handbook/*.aux handbook/*.ilg handbook/*.log handbook/*.out handbook/*.toc handbook/chapters/*.aux

metrics : $(hses) $(jses) $(csses)
	cloc $(hses) $(jses) $(csses)
	ls -l js/compiled/*.js
	ls -l css/compiled/*.css
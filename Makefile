# Vocabulink site

cgi := vocabulink.cgi
hses := cgi/Vocabulink.hs $(shell find cgi/Vocabulink -name "*.hs")
sasses := $(shell find -name "*.sass" | grep -v lib.sass)
csses := $(sources:.sass=.css)
csslibs := css/lib.common.css css/lib.link.css css/lib.member.css
jses := $(shell find js -maxdepth 1 -name "*.js")
minjses := js/compiled/common.js js/compiled/link.js js/compiled/member.js js/compiled/raphael.js js/compiled/metrics.js js/compiled/review.js js/compiled/dashboard.js
muses := $(shell find -name "*.muse")
articles := $(muses:.muse=.html)
chapters:= $(shell ls handbook/chapters/*.tex)

sync_options := -avz --exclude 'cgi/dist' --exclude 'upload/img/*' --exclude 'upload/audio/pronunciation/*' --exclude '*.sass' --exclude 'articles/Makefile' --exclude '*.el' --exclude 'css/Makefile' --exclude 'js/Makefile' --exclude 'cgi/*.pdf' --exclude 'cgi/TAGS' --exclude '*.aux' --exclude '*.tex' --exclude '*.ptb' --exclude '*.log' --exclude '*.out' --exclude '._*' --exclude '.DS_Store' --exclude '.sass-cache' --delete articles css etc img js s scripts offline vocabulink.cgi vocabulink.com:vocabulink/

all : $(cgi) css js articles handbook offline

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

css : $(csses) $(csslibs)

css/lib.common.css : css/common.css css/comment.css css/jquery.toastmessage.css css/jquery-loadmask.css css/jquery.simplemodal.css
	cat $^ > $@

css/lib.link.css : css/link.css
	cat $^ > $@

css/lib.member.css : css/link-editor.css css/markitup-set.css css/markitup-skin.css css/fileuploader.css
	cat $^ > $@

%.css : %.sass css/lib.sass
	sass $< > $@

js : $(minjses)

# This is getting large. I'd like to break it up and do deferred loading at some point.
js/compiled/common.js : js/jquery-1.6.1.js js/jquery.cookie.js js/minform.js js/jquery.loadmask.js js/jquery.toastmessage.js js/common.js js/jquery.simplemodal-1.4.1.js
	cat $^ | jsmin > $@

# link viewing, not editing
js/compiled/link.js : js/link.js js/longtable.js
	cat $^ | jsmin > $@

# This is for email-verified members.
js/compiled/member.js : js/jquery.markitup.js js/markdown.set.js js/showdown.js js/ajax.js js/chartviz.js js/comment.js js/link-editor.js
	cat $^ | jsmin > $@

js/compiled/review.js : js/review.js js/jquery.hotkeys.js
	cat $^ | jsmin > $@

js/compiled/raphael.js : js/raphael.js
	cat $^ | jsmin > $@

js/compiled/metrics.js : js/metrics.js
	cat $^ | jsmin > $@

js/compiled/dashboard.js : js/drcal.js js/dashboard.js
	cat $^ | jsmin > $@

# TODO: Switch these to git submodules.
js/chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@

js/minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

js/longtable.js : /home/jekor/project/longtable/longtable.js
	cp $^ $@

js/drcal.js : /home/jekor/project/drcal/drcal.js
	cp $^ $@

handbook : handbook/handbook.pdf

handbook/handbook.pdf : handbook/handbook.tex $(chapters)
	cd handbook && xelatex handbook

articles : $(articles)

%.html : %.muse articles/muse-init.el
	emacs -q -batch -l articles/muse-init.el -f muse-batch-publish-files xhtml $<

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

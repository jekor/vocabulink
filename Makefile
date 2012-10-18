# Vocabulink site

cgi := vocabulink.cgi
all : $(cgi) js css spritesheets articles handbook

# Haskell

hses := cgi/Vocabulink.hs $(shell find cgi/Vocabulink -name "*.hs")

cgi : $(cgi)

cgi/dist/setup-config : cgi/vocabulink.cabal
	cd cgi && cabal configure

cgi/dist/build/$(cgi)/$(cgi) : cgi/dist/setup-config $(hses)
	cd cgi && TPG_DB="vocabulink" TPG_USER="vocabulink" cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)

$(cgi) : cgi/dist/build/$(cgi)/$(cgi)
	if [ -f $(cgi) ]; then mv $(cgi) $(cgi).old; fi
	cp $^ $@
	strip $@

# JavaScript

jslibs := common link member dashboard learn member-page
# Common is getting large. I'd like to break it up and maybe do deferred loading at some point.
js_common := external/jquery external/jquery.cookie external/minform external/jquery.loadmask external/jquery.toastmessage external/jquery.simplemodal common loggedout
js_link := external/longtable link
js_member := external/jquery.markitup external/markdown.set external/showdown loggedin ajax comment
js_dashboard := external/drcal dashboard
js_learn := external/jquery.hotkeys external/jquery.easing learn
js_member-page := member-page

define js_template =
js/compiled/$(1).js : $$(js_$(1):%=js/%.js)
	cat $$^ | jsmin > $$@
JS += js/compiled/$(1).js
endef

$(foreach jslib,$(jslibs),$(eval $(call js_template,$(jslib))))

js : $(JS)

js/external/minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

js/external/longtable.js : /home/jekor/project/longtable/longtable.js
	cp $^ $@

js/external/drcal.js : /home/jekor/project/drcal/drcal.js
	cp $^ $@

# TODO: Add command to fetch jquery, jquery plugins, showdown, etc. into js/external/

# CSS

csslibs := common member link article dashboard member-page front learn
css_common := common comment external/jquery.toastmessage external/jquery-loadmask external/jquery.simplemodal
css_common_css := spritesheet/icon spritesheet/toast
css_member := external/markitup
css_member_css := spritesheet/markitup
css_link := link
css_article := article
css_dashboard := dashboard
css_member-page := member-page
css_front := front
css_learn := learn

define css_template =
css/compiled/$(1).css : $$(css_$(1):%=css/%.sass) $$(css_$(1)_css:%=css/%.css)
	cat css/lib.sass $$(css_$(1):%=css/%.sass) | sass | cat - $$(css_$(1)_css:%=css/%.css) > $$@
CSS += css/compiled/$(1).css
endef

$(foreach csslib,$(csslibs),$(eval $(call css_template,$(csslib))))

css : $(CSS)

# Spritesheets

spritedirs := $(shell find img/* -type d)
sprites := $(notdir $(spritedirs))
spritesheets := $(addprefix css/spritesheet/,$(addsuffix .css,$(sprites)))

spritesheets : $(spritesheets)

define spritesheet_template
css/spritesheet/$(1).css : img/$(1)
	glue img/$(1) img --url=/img/
	optipng img/$(1).png
	mv img/$(1).css css/spritesheet/
endef

$(foreach sprite,$(sprites),$(eval $(call spritesheet_template,$(sprite))))

# Documents

markdowns := $(shell find -name "*.markdown")
articles := $(markdowns:.markdown=.html)
articles : $(articles)

%.html : %.markdown articles/template.html
	pandoc --smart --section-divs --mathjax -t html5 --toc --standalone --template=articles/template.html < $< > $@

chapters := $(shell ls handbook/chapters/*.tex)

handbook : handbook/handbook.pdf

handbook/handbook.pdf : handbook/handbook.tex $(chapters)
	cd handbook && xelatex handbook

# Directives

hlint : $(hses)
	hlint -i "Redundant do" -i "Use camelCase" $^

# For jslint, go to http://www.jslint.com/
# /*jslint browser: true, devel: true, nomen: true, plusplus: true, regexp: true, sloppy: true, vars: true, white: true, indent: 2 */

sync_options := -avz --exclude 'cgi/dist' --exclude '*.sass' --exclude '.sass-cache' --exclude '*.aux' --exclude '*.tex' --exclude '*.ptb' --exclude '*.log' --exclude '*.out' --exclude '._*' --exclude '.DS_Store' --exclude 'crontab' --exclude 'lighttpd.conf' --delete articles audio css etc img js s scripts vocabulink.cgi vocabulink.com:vocabulink/

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

clean :
	rm handbook/*.aux handbook/*.ilg handbook/*.log handbook/*.out handbook/*.toc handbook/chapters/*.aux

jses := $(shell find js -maxdepth 1 -name "*.js")

metrics : $(hses) $(jses) $(CSS)
	cloc $(hses) $(jses) $(CSS)
	ls -l js/compiled/*.js
	ls -l css/compiled/*.css

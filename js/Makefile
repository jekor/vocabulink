# Vocabulink JavaScript modules

all : lib.common.js lib.link.js lib.member.js

lib.common.js : to-function.js functional.js jquery.corner.js
	cat $^ | yuicompress --type js > $@

# link viewing, not editing
lib.link.js : raphael.js link-graph.js
	cat $^ | yuicompress --type js > $@

lib.member.js : jquery.markitup.js markdown.set.js showdown.js form.js comment.js star-rating.js link-editor.js review.js ajaxupload.js
	cat $^ | yuicompress --type js > $@

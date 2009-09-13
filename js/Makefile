# Vocabulink JavaScript modules

all : lib.common.js lib.link.js lib.member.js

lib.common.js : to-function.js functional.js common.js
	cat $^ | yuicompress --type js > $@

# link viewing and reviewing, not editing
lib.link.js : raphael.js link-graph.js review.js
	cat $^ | yuicompress --type js > $@

# This is for email-verified members.
lib.member.js : jquery.markitup.js markdown.set.js showdown.js form.js comment.js star-rating.js link-editor.js ajaxupload.js
	cat $^ | yuicompress --type js > $@

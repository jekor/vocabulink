# Vocabulink JavaScript modules

all : lib.common.js lib.link.js lib.member.js lib.upload.js

lib.common.js : to-function.js functional.js jquery.cookie.js common.js
	cat $^ | yuicompress --type js > $@

# link viewing and reviewing, not editing
lib.link.js : link.js review.js
	cat $^ | yuicompress --type js > $@

# This is for email-verified members.
lib.member.js : jquery.markitup.js markdown.set.js showdown.js form.js comment.js star-rating.js link-editor.js jquery.loadmask.js ajax.js
	cat $^ | yuicompress --type js > $@

lib.upload.js : ajaxupload.js upload.js
	cat $^ | yuicompress --type js > $@

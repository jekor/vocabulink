# Vocabulink JavaScript modules

libs := lib.common.js lib.link.js lib.member.js lib.upload.js lib.raphael.js

all : $(libs)

clean :
	rm $(libs)

lib.common.js : jquery-1.4.4.js jquery.cookie.js jquery.html5form.js common.js
	cat $^ | yuicompress --type js > $@

# link viewing and reviewing, not editing
lib.link.js : link.js review.js
	cat $^ | yuicompress --type js > $@

# This is for email-verified members.
lib.member.js : jquery.loadmask.js jquery.markitup.js markdown.set.js showdown.js fileuploader.js ajax.js chartviz.js form.js comment.js link-editor.js
	cat $^ | yuicompress --type js > $@

lib.upload.js : ajaxupload.js upload.js
	cat $^ | yuicompress --type js > $@

lib.raphael.js : raphael.js
	cat $^ | yuicompress --type js > $@

chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@
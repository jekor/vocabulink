# Vocabulink JavaScript modules

libs := lib.common.js lib.link.js lib.member.js lib.upload.js lib.raphael.js

all : $(libs)

clean :
	rm $(libs)

lib.common.js : jquery-1.6.1.js jquery.cookie.js minform.js jquery.loadmask.js jquery.toastmessage.js common.js
	cat $^ | yuicompress --type js > $@

# link viewing and reviewing, not editing
lib.link.js : link.js review.js
	cat $^ | yuicompress --type js > $@

# This is for email-verified members.
lib.member.js : jquery.markitup.js markdown.set.js showdown.js ajax.js chartviz.js comment.js link-editor.js
	cat $^ | yuicompress --type js > $@

lib.raphael.js : raphael.js
	cat $^ | yuicompress --type js > $@

chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@

minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

# Vocabulink JavaScript modules

compiled := compiled/common.js compiled/link.js compiled/member.js compiled/raphael.js compiled/metrics.js

all : $(compiled)

clean :
	rm $(compiled)

# This is getting large. I'd like to break it up and do deferred loading at some point.
compiled/common.js : jquery-1.6.1.js jquery.cookie.js minform.js jquery.loadmask.js jquery.toastmessage.js common.js jquery.simplemodal-1.4.1.js
	cat $^ | yuicompress --type js > $@

# link viewing and reviewing, not editing
compiled/link.js : link.js review.js
	cat $^ | yuicompress --type js > $@

# This is for email-verified members.
compiled/member.js : jquery.markitup.js markdown.set.js showdown.js ajax.js chartviz.js comment.js link-editor.js
	cat $^ | yuicompress --type js > $@

compiled/raphael.js : raphael.js
	cat $^ | yuicompress --type js > $@

compiled/metrics.js : metrics.js
	cat $^ | yuicompress --type js > $@

chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@

minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

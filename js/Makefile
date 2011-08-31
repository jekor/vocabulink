# Vocabulink JavaScript modules

compiled := compiled/common.js compiled/link.js compiled/member.js compiled/raphael.js compiled/metrics.js compiled/review.js

all : $(compiled)

clean :
	rm $(compiled)

# This is getting large. I'd like to break it up and do deferred loading at some point.
compiled/common.js : jquery-1.6.1.js jquery.cookie.js minform.js jquery.loadmask.js jquery.toastmessage.js common.js jquery.simplemodal-1.4.1.js
	cat $^ | jsmin > $@

# link viewing, not editing
compiled/link.js : link.js longtable.js
	cat $^ | jsmin > $@

# This is for email-verified members.
compiled/member.js : jquery.markitup.js markdown.set.js showdown.js ajax.js chartviz.js comment.js link-editor.js
	cat $^ | jsmin > $@

compiled/review.js : review.js jquery.hotkeys.js
	cat $^ | jsmin > $@

compiled/raphael.js : raphael.js
	cat $^ | jsmin > $@

compiled/metrics.js : metrics.js
	cat $^ | jsmin > $@

chartviz.js : /home/jekor/project/chartviz/chartviz.js
	cp $^ $@

minform.js : /home/jekor/project/minform/minform.js
	cp $^ $@

longtable.js : /home/jekor/project/longtable/longtable.js
	cp $^ $@

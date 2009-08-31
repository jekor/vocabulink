# Vocabulink JavaScript modules

all : lib.common.min.js lib.link.min.js lib.member.min.js

lib.%.min.js : lib.%.js
	yuicompress $^ > $@
#	jsmin < $^ > $@

lib.common.js : functional.js jquery.corner.js
	cat $^ > $@

# link viewing, not editing
lib.link.js : raphael.js link-graph.js
	cat $^ > $@

lib.member.js : jquery.markitup.js markdown.set.js showdown.js form.js comment.js star-rating.js link-editor.js review.js ajaxupload.js
	cat $^ > $@

connect(window, 'onload', setup);

function setup() {
    map(connectReply, $$('.reply'));
}

function connectReply(elem) {
    var button = findChildElements(elem, ['input[type=submit]'])[0];
    connect(button, 'onclick', partial(sendReply, elem, button));
}

function postXHR (url, postVars) {
    return doXHR(url, {'method': 'POST',
	       	       'headers': {'Content-Type': 'application/x-www-form-urlencoded'},
		       'sendContent': queryString(postVars)});
}

function sendReply(replyBox, button, e) {
    e.stop();
    setNodeAttribute(button, 'disabled', null);
    var d = postXHR('/comment/reply', formContents(replyBox));
    d.addCallbacks(partial(replySuccess, replyBox),
		   partial(replyFailure, replyBox));
}

function replySuccess(replyBox, response) {
    insertReplyForm(replyBox, evalJSONRequest(response));
}

function replyFailure(replyBox, response) {
    alert('Error posting reply.');
}

function insertReplyForm(replyBox, response) {
    if (response.html !== undefined) {
	replyBox.innerHTML = response.html;
	if (response.status === 'incomplete') {
	    var button = findChildElements(replyBox, ['input[type=submit]'])[0];
	    connect(button, 'onclick', partial(sendReply, replyBox, button));
	} else if (response.status === 'accepted') {
	    var newBox = getFirstElementByTagAndClassName('div', 'reply', replyBox);
	    var button = getFirstElementByTagAndClassName('button', null, replyBox);
	    connectButton(button);
	    connectReply(newBox);
	}
    } else {
	alert('Unable to load response form.');
    }
}

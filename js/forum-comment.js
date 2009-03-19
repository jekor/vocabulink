connect(window, 'onload', setup);

function setup() {
  map(connectReply, $$('.reply'));
}

function connectReply(elem) {
  var button = findChildElements(elem, ['input[type=submit]'])[0];
  connect(button, 'onclick', partial(sendReply, elem, button));
}

function postXHR(url, postVars) {
  return doXHR(url, {'method': 'POST',
                     'headers': {'Content-Type': 'application/x-www-form-urlencoded'},
                     'sendContent': queryString(postVars)});
}

function overlay(elem) {
  var over = DIV();
  setStyle(over, {'background-color': 'black', 'opacity': 0.5,
                  'width': '100%', 'height': '100%', 'top': '0', 'left': '0',
                  'position': 'absolute',
                  'background-image': "url('http://s.vocabulink.com/wait.gif')",
                  'background-repeat': 'no-repeat',
                  'background-position': 'center center'});
  makePositioned(elem);
  appendChildNodes(elem, over);
  return function() {removeElement(over); undoPositioned(elem);};
}

function sendReply(replyBox, button, e) {
  e.stop();
  setNodeAttribute(button, 'disabled', null);
  var remove = overlay(getFirstElementByTagAndClassName('div', 'speech', replyBox));
  var d = postXHR('/comment/reply', formContents(replyBox));
  d.addCallbacks(function(r) {replySuccess(replyBox, r); remove()},
                 function(r) {replyFailure(replyBox, r); remove()});
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

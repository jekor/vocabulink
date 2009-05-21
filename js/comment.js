// Copyright 2008, 2009 Chris Forno
//
// This file is part of Vocabulink.
//
// Vocabulink is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

connect(window, 'onload', setup);

function setup() {
  map(connectButtons, $$('.reply'));
  var toplevelComments = $$('.comment.toplevel');
  map(roundElement, toplevelComments);
}

function connectButtons(elem) {
  var button = getFirstElementByTagAndClassName('button', null, elem);
  connect(button, 'onclick', partial(previewReply, elem, button));
  var submit = findChildElements(elem, ['input[type=submit]'])[0];
  connect(submit, 'onclick', partial(sendReply, elem, button));
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

function previewReply(replyBox, button, e) {
  e.stop();
  setNodeAttribute(button, 'disabled', null);
  var remove = overlay(getFirstElementByTagAndClassName('div', 'speech', replyBox));
  var speech = getFirstElementByTagAndClassName('div', 'speech', replyBox);
  var t = getFirstElementByTagAndClassName('textarea', null, speech);
  var d = loadJSONDoc('/comment/preview', {'comment': t.value});
  d.addCallbacks(function(r) {previewSuccess(speech, button, r); remove();},
                 function(r) {previewFailure(speech, button, r); remove();});
}

function sendReply(replyBox, button, e) {
  e.stop();
  setNodeAttribute(button, 'disabled', null);
  var remove = overlay(getFirstElementByTagAndClassName('div', 'speech', replyBox));
  var d = postXHR('/comment/reply', formContents(replyBox));
  d.addCallbacks(function(r) {replySuccess(replyBox, r); remove()},
                 function(r) {replyFailure(replyBox, r); remove()});
}

function previewSuccess(speech, button, response) {
  if (response.status == 'OK' && response.html !== undefined) {
    var editButton = BUTTON('Edit');
    hideElement(button);
    button.removeAttribute('disabled');
    insertSiblingNodesAfter(button, editButton);
    speechPreview = DIV({'class': 'speech'});
    hideElement(speech);
    insertSiblingNodesAfter(speech, speechPreview);
    speechPreview.innerHTML = response.html;
    connect(editButton, 'onclick', function(e) {
      e.stop();
      button.innerHtml = 'Preview';
      removeElement(speechPreview);
      showElement(speech);
      removeElement(editButton);
      // We can't use showElement, because it assumes display: block.
      setDisplayForElement('inline', button);
    });
  }
}

function previewFailure(speech, response) {
  alert('Error generating preview.');
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

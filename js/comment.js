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

$(document).ready(function() {
  $('.reply').each(connectButtons);
  $('.comment.toplevel').corner();
  $('.vote-arrow').click(vote);
});

function connectButtons() {
  var button = $(this).find('button:first');
  var submit = $(this).find('input[type=submit]:first');
  button.click(previewReply.curry($(this), button));
  submit.click(sendReply.curry($(this), button));
}

function overlay(elem) {
  elem.css('position', 'relative');
  var over = $('<div style="background-color: black; opacity: 0.5; width: 100%; height: 100%; top: 0; left: 0; position: absolute; background-image: url(\'http://s.vocabulink.com/wait.gif\'); background-repeat: no-repeat; background-position: center center;"></div>').insertAfter(elem);
  return function() { over.remove(); elem.css('position', 'inherit'); };
}

function previewReply(replyBox, button, e) {
  e.preventDefault();
  button.attr('disabled', null);
  var speech = replyBox.find('div.speech:first');
  var remove = overlay(speech);
  var t = speech.find('textarea:first');
  $.ajax({'type': 'GET', 'url': '/comment/preview',
          'data': {'comment': t.val()},
          'dataType': 'json',
          'success': function(data) {
            previewSuccess(speech, button, data);
            remove();
          },
          'error':   function(data) {
            previewFailure(speech, button, data);
            remove();
          }});
}

function sendReply(replyBox, button, e) {
  e.preventDefault();
  button.attr('disabled', null);
  var speech = replyBox.find('div.speech:first');
  var remove = overlay(speech);
  $.ajax({'type': 'POST', 'url': '/comment/reply',
          'data': replyBox.find('form').serialize(),
          'dataType': 'json',
          'success': function(data) {
            replySuccess(replyBox, data);
            remove();
          },
          'error':   function(data) {
            replySuccess(replyBox, data);
            remove();
          }});
}

function previewSuccess(speech, button, data) {
  if (data.status == 'OK' && data.html !== undefined) {
    button.hide();
    button.removeAttr('disabled');
    var editButton = $('<button>Edit</button>').insertAfter(button);
    speech.hide();
    var speechPreview = $('<div class="speech">' + data.html + '</div>').insertAfter(speech);
    editButton.click(function(e) {
      e.preventDefault();
      button.text = 'Preview';
      speechPreview.remove();
      speech.show();
      editButton.remove();
      // We can't use show(), because it assumes display: block.
      button.css('display', 'inline');
    });
  }
}

function previewFailure(speech, data) {
  alert('Error generating preview.');
}

function replySuccess(replyBox, data) {
  insertReplyForm(replyBox, data);
}

function replyFailure(replyBox, data) {
  alert('Error posting reply.');
}

function insertReplyForm(replyBox, data) {
  if (data.html !== undefined) {
    replyBox.html(data.html);
    if (data.status === 'incomplete') {
      var button = replyBox.find('input[type=submit]:first');
      button.click(sendReply.curry(replyBox, button));
    } else if (data.status === 'accepted') {
      var newBox = replyBox.find('div.reply:first');
      var button = replyBox.find('button:first');
      connectButton(button);
      connectReply(newBox);
    }
  } else {
    alert('Unable to load response form.');
  }
}

function vote(e) {
  e.preventDefault();
  var arrow = $(this);
  var parent = arrow.parent();
  if (!parent.hasClass('enabled'))
    return;
  var voteCount = parent.find('span:first');
  var count = parseInt(voteCount.text());
  var url = arrow.attr('href') + '/votes';
  var fail = function() { voteCount.text('FAIL!'); };
  if (arrow.hasClass('up')) {
    $.ajax({'type': 'POST', 'url': url,
            'data': {'vote': 'up'},
            'success': function() {
              voteCount.text(count + 1);
              arrow.css('background-position', '4px -24px');
              parent.removeClass('enabled');
            },
            'error': fail});
  } else if (arrow.hasClass('down')) {
    $.ajax({'type': 'POST', 'url': url,
            'data': {'vote': 'down'},
            'success': function() {
              voteCount.text(count - 1);
              arrow.css('background-position', '4px -37px');
              parent.removeClass('enabled');
            },
            'error': fail});    
  }
}

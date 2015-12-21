(function ($) {

function createCommentBox(parentID) {
  var box = $(
    '<form method="post" class="comment">'
    + '<div class="metadata">'
      + '<span class="username"></span>'
    + '</div>'
    + '<img class="avatar" width="48" height="48">'
    + '<input type="submit" value="Send" class="send light"></input>'
    + '<div class="speech-bubble left body">'
      + '<textarea name="body" required placeholder="Add your comment here."></textarea>'
    + '</div>'
  + '</form>');
  box.attr('action', '/comment/' + parentID + '/reply');
  box.find('.username').text(V.member.name);
  box.find('.avatar').attr('src', V.memberGravatar(48));
  return box.minform();
}

// This box is for replying to comments.
function createReplyBox(parentNumber) {
  return createCommentBox(parentNumber);
}

// Replying to comments, including generating the necessary form, is left up to
// JavaScript. This creates a box similar to a comment box (which is generated
// by the backend CGI) but with the additional machinery necessary for sending
// the reply.
// The commentBox passed is the one we're replying to. We'll use it to get the
// target comment number and for relative placement of the new box. commentBox
// should be a jQuery object.
function addReplyCommentBox(commentBox) {
  var parentIndent = parseFloat(commentBox.attr('style').match(/margin-left: ([0-9.]+)em/i)[1]);
  if (parentIndent === undefined || isNaN(parentIndent)) {
    alert('Unexpected error. Unable to determine comment level.');
    return false;
  }
  var commentNumber = parseInt(commentBox.attr('comment'), 10);
  if (commentNumber === undefined || isNaN(commentNumber)) {
    alert('Unexpected error. Unable to determine comment number.');
    return false;
  }
  // Set the indent 1 level deeper than commentBox.
  var indent = parentIndent + 1.3;
  createReplyBox(commentNumber).css('margin-left', indent + 'em').insertAfter(commentBox);
  // TODO: Highlight the new box, auto-focus the textarea, or both.
  return false;
}

// Pages that allow comments have a root comment to which new comments are sent
// in reply to by default. Sometimes the root comment is visible, sometimes
// it's an invisible blank comment. In any case, we want to create a comment box
// in those cases. We'll look for a rootComment variable to do so.
function setupRootReply() {
  var rootComment = parseInt($(this).attr('id').match(/comments-(\d+)/i)[1], 10);
  createCommentBox(rootComment).appendTo($(this));
}

$(function () {
  $('.comment .reply').click(function () {
    addReplyCommentBox($(this).parent());
  });
  if (V.verified()) {
    $('.comments').each(setupRootReply);
  }
});

})(jQuery);

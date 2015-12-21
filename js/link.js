(function ($) {

// Local Storage
//
// Warning: For large keys (> 100KB) such as 'retain', fetching, deserializing,
// reserializing, and then storing objects can take ~20ms (tested in Firefox).
// Do operations on them in batches.
//
// An individual link is stored in an "object" (an array to keep it more compact):
// type link = [[learn, learnLang], [known, knownLang], soundalike, linkword]
//
// Every word (link) that we've encountered as a learner is retained in this
// list. It's retained here so that, for example, we can de-highlight words
// we've already encountered in a story:
// retain => {linkNumber: link, ...}
//
// The server gives us a list of upcoming reviews. This list should be a subset
// of retain:
// pendingReviews => {linkNumber: targetTs, ...}
//
// Every time we review a link we store the information on the review in a list
// in case we're unable to immediately send the information to the server:
// reviews => [[linkNumber, recallGrade, recallTime, ts], ...]

// Given a link object, "retain" it if we haven't done so already. This entails
// putting the link into local storage and notifying the server that we want to
// review this link in the future.
//
// If the link given has already been retained previously, we do nothing and
// return false. If it was just retained in this call for the first time, we
// notify the server and return true.
V.retainLink = function (linkNumber) {
  if (V.insertLocal('retain', linkNumber)) {
    // The link hasn't been retained previously. Notify the server if logged in.
    if (V.loggedIn()) {
      // TODO: What if this fails? Explain how sync works or have a backup plan.
      $.ajax('/review/' + linkNumber, {'type': 'PUT'});
    }
    return true;
  } else {
    return false;
  }
};

// Increment (or decrement, with a negative number) the "words to review" count in the header.
V.incrReviewCount = function (by) {
  var el = $('.review-box strong');
  V.setReviewCount(V.getReviewCount() + by);
};

V.getReviewCount = function () {
  var el = $('.review-box strong');
  return parseInt(el.text(), 10);
};

V.setReviewCount = function (to) {
  var el = $('.review-box strong');
  el.css('opacity', 1);
  el.animate({'opacity': 0}, 'fast', 'swing', function () {
    $('.review-box').empty().append('<strong style="opacity: 0">' + to + '</strong> ' + (to == 1 ? 'word' : 'words') + ' to review');
    $('.review-box strong').animate({'opacity': 1}, 'fast', 'swing');
  });
};

V.annotateLink = function (link) {
  link.children('.foreign, .familiar, .link').each(function () {
    var word = $(this);
    if (word.attr('title')) {
      var caption = $('<span class="caption"></span>').text(word.attr('title'));
      // We have to calculate these before we add content to them and screw up
      // the dimensions.
      var width = word.outerWidth();
      var y = (word.hasClass('foreign') || word.hasClass('familiar')) ? word.outerHeight() + 4 : word.height() + 8;
      caption.appendTo(word);
      var x = (width - caption.width()) / 2;
      caption.css({'position': 'absolute', 'left': x, 'top': y});
    }
  });
};

function linkNumber() {
  return window.location.pathname.split('/').pop();
}

function collapseStory(q, h, mh) {
  q.addClass('collapsed').css('height', mh);
  var readMore = $('<div class="control"><a href="" class="read-more">read more...</a></div>');
  readMore.find('a').click(function () {
    var readLess = $('<a href="" class="read-less">read less...</a>');
    readLess.click(function () {
      q.animate({'height': mh}, 750, function () {
        readMore.remove();
        collapseStory(q, h, mh);
      });
    });
    q.animate({'height': h}, 750, function () {
      q.removeClass('collapsed');
      readMore.find('a').replaceWith(readLess);
    });
    return false;
  });
  q.after(readMore);
}

function showNewStory() {
  if (!V.verified()) {
    $('<div class="linkword-story-container">'
      + '<a class="verified" href="/member/confirmation">Verify Email to Add Story</a>'
    + '</div>').appendTo('#linkword-stories');
    return;
  }
  var newStory =
    $('<div class="linkword-story-container">'
      + '<div class="linkword-story">'
        + '<form method="post">'
          + '<blockquote>'
            + '<textarea name="story" required placeholder="Add your own story here."></textarea>'
          + '</blockquote>'
        + '</form>'
      + '</div>'
    + '</div>');
  newStory.find('form').attr('action', '/link/' + linkNumber() + '/stories');
  newStory.appendTo('#linkword-stories');
  newStory.find('textarea').one('focus', function () {
    $(this).animate({'height': '10em'}, 250, function () {
      $(this).markItUp(V.markItUpSettings);
      $(this).select().focus();
      $(this).parents('form').append(
        '<div class="signature">'
        + '<input type="submit" class="save light" value="Save Story"></input>'
        + '<button class="cancel">cancel</button>'
        + '<div class="clear"></div>'
      + '</div>');
      newStory.find('.cancel').click(function () {
        newStory.remove();
        showNewStory();
        return false;
      });
    });
  });
  $('form', newStory).minform();
}

$(function () {
  V.annotateLink($('h1.link:visible'));

  $('.linkword-story blockquote').each(function () {
    if ($(this).height() > 140) {
      collapseStory($(this), $(this).height(), 140);
    }
  });

  if (V.loggedIn() && $('#linkword-stories').length) {
    showNewStory();
    $('.linkword-story').each(function () {
      // Quick hack to allow "admin" edits.
      if ($(this).find('.username').text() === V.member.name || V.member.name === 'jekor') {
        var sig = $(this).find('.signature');
        var editButton = $('<button class="light">Edit</button>');
        editButton.click(function () {
          var story = $(this).parents('.linkword-story');
          var storyNumber = parseInt(story.parent().find('a:first').attr('id'), 10);
          story.mask('Loading...');
          var body = $.ajax({ 'type': 'GET'
                            , 'url': '/link/story/' + storyNumber
                            , 'success': function (data) {
                              story.unmask();
                              story.hide();
                              var form = $(
                                '<form class="linkword-story" method="post">'
                                + '<blockquote>'
                                  + '<textarea name="story" required></textarea>'
                                + '</blockquote>'
                                + '<div class="signature">'
                                  + '<input type="submit" value="Save" class="light">'
                                  + '<button class="cancel">cancel</button>'
                                + '</div>'
                              + '</form>').insertAfter(story);
                              form.attr('action', '/link/story/' + storyNumber);
                              form.find('textarea').text(data);
                              form.minform();
                              form.find('textarea').css('height', '10em').markItUp(V.markItUpSettings);
                              form.find('.cancel').click(function () {
                                form.remove();
                                story.show();
                              });
                            }
                            , 'error': function () {
                              story.unmask();
                              alert('Error retrieving story.');
                            }
                           });
        });
        sig.prepend(editButton);
      }
    });
  }
});

})(jQuery);

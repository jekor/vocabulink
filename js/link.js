// Copyright 2009, 2010, 2011, 2012 Chris Forno
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

(function ($) {

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
      $(this).markItUp(mySettings);
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
      if ($(this).find('.username').text() === V.memberName || V.memberName === 'jekor') {
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
                              form.find('textarea').css('height', '10em').markItUp(mySettings);
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

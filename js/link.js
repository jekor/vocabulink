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
      if (word.hasClass('foreign') || word.hasClass('familiar')) {
        var y = word.outerHeight() + 4;
      } else {
        var y = word.height() + 8;
      }
      caption.appendTo(word);
      var x = (width - caption.width()) / 2;
      caption.css({'position': 'absolute', 'left': x, 'top': y});
    }
  });
}

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

function editFrequency() {
  // Grab the existing frequency lists.
  var lang = $('h1 .foreign').attr('lang');
  $.get('/list/frequency/' + lang)
   .done(function (lists) {
     var editor = $(
       '<form id="frequency-editor" method="post" style="display: none">'
       + '<button id="new-freq-list" class="light">New List</button> '
       + '<label>Rank:</label> <input name="rank" size="4" required> '
       + '<label>Frequency:</label> <input name="frequency" required> '
       + '<input class="light" type="submit" value="Set Frequency">'
     + '</form>').appendTo('#link-head-bar').slideDown();
     var select = $('<select name="list" required style="min-width: 10em"></select>').prependTo(editor);
     $.each(lists, function (_, list) {
       var option = $('<option></option>').appendTo(select);
       option.val(list.number).text(list.name);
     });
     editor.attr('action', '/link/' + linkNumber() + '/frequencies');
     editor.minform().submit(function (e) {
       e.preventDefault();
       editor.mask('Sending...');
       $.post($(this).attr('action'), $(this).serialize())
        .done(function () {
          window.location.reload();
        })
        .fail(function (xhr) {
          editor.unmask();
          V.toastError(xhr.responseText, true);
        });
       return false;
     });
     editor.find('#new-freq-list').click(function () {
       var content = $(
         '<div><h1>New Frequency List</h1>'
         + '<form method="post">'
           + '<label>Name:</label> <input name="name" required><br>'
           + '<label>Description:</label> <input name="description" required>'
           + '<input type="submit" class="light" value="Add List">'
         + '</form>'
       + '</div>');
       content.find('form').attr('action', '/list/frequency/' + lang);
       $.modal(content);
       var modal = $('#simplemodal-container');
       content.find('form').minform().submit(function (e) {
         e.preventDefault();
         modal.mask('Sending...');
         $.post($(this).attr('action'), $(this).serialize())
          .done(function () {
            $.modal.close();
            V.toastSuccess('List created successfully');
            $('#frequency-editor').remove();
            editFrequency();
          })
          .fail(function (xhr) {
            modal.unmask();
            V.toastError(xhr.responseText, true);
          });
         return false;
       });
       return false;
     });
   });
}

$(function () {
  V.annotateLink($('h1.link:visible:not(.edit)'));

  $('#pronounce').click(function () {
    $(this).find('audio')[0].play();
    return false;
  });

  $('.linkword-story blockquote').each(function () {
    if ($(this).height() > 140) {
      collapseStory($(this), $(this).height(), 140);
    }
  });

  // Quick hack to allow "admin" edits.
  if (V.memberName === 'jekor') {
    $('#rank').click(editFrequency);
    var edit = function () {
      var $this = $(this);
      var save = $this.clone();
      var text = $this.clone().children().remove().end().text();
      var input = $('<input>').attr('value', text);
      var cancel = function () {
        $this.replaceWith(save);
        save.click(edit);
      };
      input.keyup(function (e) {
        if (e.keyCode === 27) { // Esc
          cancel();
        }
				if (e.keyCode === 13 || e.keyCode === 10) { // Enter
          $this.mask('Saving...');
          if ($this.hasClass('foreign')) {
            var part = 'foreign';
          } else if ($this.hasClass('familiar')) {
            var part = 'familiar';
          } else if ($this.hasClass('link')) {
            var part = 'linkword';
          } else {
            return false;
          }
          $.ajax({'type': 'PUT',
                  'url':  window.location.pathname + '/' + part,
                  'data': input.val(),
                  'contentType': 'text/plain'})
           .done(function () {
             window.location.reload();
           })
           .fail(function (xhr) {
             $this.unmask();
             V.toastError(xhr.responseText, true);
           });
        }
      })
      input.focusout(cancel);
      $this.empty().append(input);
      input.focus().select();
    }
    $('.link:not(.edit) .foreign, .link:not(.edit) .familiar, .link:not(.edit) .link').one('click', edit);
  }

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

  // "add to review"
  $('#link-op-review.enabled').click(function (e) {
    var op = $(this);
    op.mask("Adding...");
    var linkNum = window.location.pathname.split('/').pop();
    $.ajax('/review/' + linkNum, {'type': 'PUT'})
     .done(function () {
       op.unmask();
       op.text("now reviewing");
       V.incrLinksToReview(1);
     })
     .fail(function (xhr) {
       op.unmask();
       op.addClass("failed");
       op.text("Failed!");
       V.toastError(xhr.responseText);
     });
    return false;
  });

  // "delete link"
  $('#link-op-delete.enabled').click(function () {
    var op = $(this);
    op.mask('Deleting...');
    $.ajax({'type': 'DELETE'
           ,'url': '/link/' + linkNumber()
           ,'success': function () {
             op.text('Deleted.');
           }
           ,'error': function () {
             op.addClass('failed');
             op.text('Failed!');
           }
          });
    return false;
  });
});

})(jQuery);
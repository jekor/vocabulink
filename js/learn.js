// Copyright 2012 Chris Forno
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
  // Called when the #confirm button is clicked.
  var confirm = function () {};
  var nextAction = function () {};
  var recallTime = 0;

  function header(text) {
    $('#learn-header h2').text(text);
  }

  function link() {
    return $(
      '<h1 class="link association" linkno="">'
      + '<span class="foreign">'
        + '<span class="foreign-word"></span>'
        + '<button class="pronounce button light">'
          + '<audio>'
            + '<source src=""></source>'
            + '<source src=""></source>'
          + '</audio>'
          + '<img src="//s.vocabulink.com/img/icon/audio.png">'
        + '</button>'
      + '</span>'
      + '<span class="link">'
      + '</span>'
      + '<span class="familiar">'
        + '<span class="familiar-word"></span>'
      + '</span>'
    + '</h1>');
  }

  function stories(ss) {
    return $(
      '<div id="linkword-stories">'
      + '<div class="header">'
        + '<h2>Linkword Stories:</h2>'
      + '</div>'
    + '</div>');
  }

  function actionArea() {
    var box = $(
      '<div id="action-area">'
      + '<button id="confirm" class="faint-gradient-button green" title="hotkey: enter"></button>'
    + '</div>');
    box.append(gradeBar().hide());
    return box;
  }

  function grade(g) {
    $('#grades').hide();
    $.ajax('/review/' + $('h1').attr('linkno')
          ,{'type': 'POST'
           ,'data': {'grade': g
                    ,'time': recallTime}})
     .fail(function () {V.toastError('Failed to record grade.');});
    if (g >= 0.5) {
      // Update the "X links to review" in the header.
      V.incrLinksToReview(-1);
    }
    nextAction();
  }

  function gradeBar() {
    var grades = $('<div id="grades"></div>');
    $.each(['blank', 'wrong', 'almost', 'barely', 'good', 'perfect'], function (i, text) {
      var button = $('<button><b></b><br></button>');
      button.addClass('grade' + i).attr('grade', i / 5).attr('title', 'hotkey: ' + (i + 1));
      button.append(text);
      button.click(function () {grade(i / 5);});
      $(document).bind('keyup', (i + 1).toString(), function () {
        if ($('#grades:visible').length > 0) {
          button.addClass('pressed');
          setTimeout(function () {
            button.removeClass('pressed');
            grade(i / 5);
          }, 250);
        }
      });
      grades.append(button);
    });
    return grades;
  }

  function updateLink(el, link) {
    el.attr('linkno', link[0]);
    var sources = $('audio', el);
    $(sources[0]).attr('src', '//s.vocabulink.com/audio/pronunciation/' + link[0] + '.ogg');
    $(sources[1]).attr('src', '//s.vocabulink.com/audio/pronunciation/' + link[0] + '.mp3');
    $('.foreign-word', el).text(link[1]);
    $('.familiar-word', el).text(link[2]);
    el.removeClass('linkword').removeClass('soundalike').removeClass('association');
    if (link[3]) {
      if (typeof link[3].linkword != 'undefined') {
        el.addClass('linkword');
        $('.link', el).attr('title', 'linkword');
        $('.link', el).text(link[3].linkword);
      } else if (typeof link[3].soundalike != 'undefined') {
        el.addClass('soundalike');
        $('.link', el).attr('title', 'soundalike');
        $('.link', el).empty();
      } else {
        el.addClass('association');
        $('.link', el).attr('title', '');
        $('.link', el).empty();
      }
    } else {
      $('.link', el).attr('title', '');
      $('.link', el).empty();
      el.addClass('association');
    }
  }

  function updateStories(el, link) {
    $('.linkword-story-container', el).remove();
    $('<p class="loading">Loading Stories...</p>').appendTo('#linkword-stories');
    $.get('/link/' + link[0] + '/stories')
     .done(function (html) {
       $('#linkword-stories > p').remove();
       $('.linkword-story-container', el).remove();
       el.append($(html));
     })
     .fail(function (xhr) {
       $('#linkword-stories > p').remove();
       V.toastError(xhr);
     });
  }

  function addToReview(linkNum) {
    if (V.loggedIn()) {
      $.ajax('/review/' + linkNum, {'type': 'PUT'})
       .done(function () {
         V.incrLinksToReview(1);
       })
       .fail(function (xhr) {
         V.toastError(xhr.responseText);
       });
    } else {
      var queue = V.getLocal('learnQueue', []);
      queue.push(linkNum);
      V.setLocal('learnQueue', V.uniq(queue));
    }
  }

  function fetchReviews() {
    // Don't re-add items that have been reviewed between now...
    var currentLinkNums = $.map(review, function (_, link) {
      return link[0];
    });
    currentLinkNums.push(parseInt($('h1').attr('linkno'), 10));
    $.get('/learn/reviews' + location.search)
     .done(function (links) {
       // Randomize the links so that the learner doesn't get too used to the
       // order they arrive in.
       links.sort(function () {return Math.round(Math.random()) - 0.5;});
       // ...and now. Because they might have been reviewed in the meantime.
       review = review.concat($.grep(links, function (link) {
         return $.inArray(link[0], currentLinkNums) === -1;
       }));
     })
     .fail(function (xhr) {
       // Fail silently for now.
     });
  }

  function fetchLearns(nextAction) {
    // Don't re-add items that have been learned between now...
    var currentLinkNums = $.map(review, function (_, link) {
      return link[0];
    });
    currentLinkNums.push(parseInt($('h1').attr('linkno'), 10));
    $.get('/learn/new' + location.search)
     .done(function (links) {
       // ...and now. Because they might have been reviewed in the meantime.
       learn = learn.concat($.grep(links, function (link) {
         return $.inArray(link[0], currentLinkNums) === -1;
       }));
       if (learn.length > 0) {
         nextAction();
       } else {
         // TODO: The user is done with this language. We have no more words
         // for them to learn.
       }
     })
     .fail(function (xhr) {
       V.toastError('Failed to fetch more words to learn.')
     });
  }

  $(function () {
    var linkEl = link().hide().appendTo('#body');
    var actionEl = actionArea().hide().appendTo('#body');
    var storiesEl = stories().hide().appendTo('#body');
    $('#confirm').click(function () {confirm();});
    $(document).bind('keyup', 'return', function () {
      var button = $('#confirm');
      button.addClass('pressed');
      setTimeout(function () {
         button.removeClass('pressed');
         confirm();
      }, 200);
    });
    nextAction = function () {
      if (review.length > 0) {
        if (review.length == 1) {
          fetchReviews();
        }
        doReview(review.pop());
      } else if (learn.length > 0) {
        doLearn(learn.pop());
      } else {
        // Clear the learning area.
        $('h1, #action-area, #linkword-stories').hide();
        if (V.loggedIn()) {
          header('Loading More Words...');
          fetchLearns(nextAction);
        } else {
          // Display an invitation to join the site.
          header('Sign Up to Continue');
          $('#signup-invitation').show();
          $('#signup-button').click();
        }
      }
    };
    var doReview = function (link) {
      var answer = link[2];
      var extra = link[3];
      link[2] = '?';
      delete link[3];
      linkEl.hide();
      updateLink(linkEl, link);
      header('Remember This Word?');
      actionEl.show();
      linkEl.show();
      var recallStart = Date.now();
      $('#confirm').text('Reveal Answer').show();
      confirm = function () {
        recallTime = Date.now() - recallStart;
        $('#confirm').hide();
        link[2] = answer;
        link[3] = extra;
        updateLink(linkEl, link);
        $('#grades').show();
        header('How Well Did You Remember?');
      };
    }
    var doLearn = function (link) {
      linkEl.hide();
      updateLink(linkEl, link);
      updateStories(storiesEl, link);
      header('Study This Word');
      actionEl.show();
      $('#confirm').text('OK, Got it').show();
      confirm = function () {
        addToReview(link[0]);
        nextAction();
      };
      linkEl.show();
      storiesEl.show();
    }

    // Remove any links from the learn list that are already in local storage.
    var queue = V.getLocal('learnQueue', []);
    learn = $.grep(learn, function (link) {
      return $.inArray(link[0], queue) === -1;
    });

    nextAction();
    // Check for new links to review every 10 minutes.
    if (V.loggedIn()) {
      setInterval(function () {
        // Are there already items to review?
        if (review.length <= 1) {
          fetchReviews();
        }
      }, 1000 * 60 * 10);
    }
  });

})(jQuery);

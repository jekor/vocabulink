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
  var firstLink = null; // Used for first timers.

  function header(text) {
    $('#learn-header h2').text(text);
  }

  function link() {
    return $(
      '<h1 class="link association" linkno="">'
      + '<span class="foreign" title="' + learnLanguage + '">'
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
      + '<span class="familiar" title="' + knownLanguage + '">'
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
      + '<button id="confirm" class="faint-gradient-button green" title="hotkey: enter or space"></button>'
    + '</div>');
    box.append(gradeBar().hide());
    return box;
  }

  function grade(g) {
    $('#grades').hide();
    if (!V.loggedIn()) {
      firstLink = null;
      $('.tip').remove();
    } else {
      $.ajax('/review/' + $('h1.link').attr('linkno')
            ,{'type': 'POST'
             ,'data': {'grade': g
                      ,'time': recallTime}})
       .fail(function () {V.toastError('Failed to record grade.');});
      if (g >= 0.5) {
        // Update the "X links to review" in the header.
        V.incrLinksToReview(-1);
      }
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
       V.toastMessage('error', xhr);
     });
  }

  function addToReview(linkNum) {
    if (V.loggedIn()) {
      $.ajax('/review/' + linkNum, {'type': 'PUT'})
       .done(function () {
         V.incrLinksToReview(1);
       })
       .fail(function (xhr) {
         V.toastMessage('error', xhr.responseText);
       });
    } else {
      var queue = V.getLocal('learnQueue', []);
      queue.push(linkNum);
      V.setLocal('learnQueue', V.uniq(queue));
    }
  }

  function fetchUpcoming(nextAction) {
    // Don't re-add items that have been reviewed between now...
    var currentLinkNums = $.map(review, function (_, link) {
      return link[0];
    });
    currentLinkNums.push(parseInt($('h1.link').attr('linkno'), 10));
    $.get('/learn/upcoming' + location.search + '&n=10') // location.search preserves the learn/known parameters
     .done(function (links) {
       if (links.review.length > 0) {
         // Randomize the links so that the learner doesn't get too used to the
         // order they arrive in.
         links.review.sort(function () {return Math.round(Math.random()) - 0.5;});
         // ...and now. Because they might have been reviewed in the meantime.
         review = review.concat($.grep(links.review, function (link) {
           return $.inArray(link[0], currentLinkNums) === -1;
         }));
         nextAction();
       }
       if (links.learn.length > 0) {
         learn = learn.concat($.grep(links.learn, function (link) {
           return $.inArray(link[0], currentLinkNums) === -1;
         }));
       }
       if (links.review.length === 0 && links.learn.length === 0) {
         // TODO: The user is done with this language. We have no more words
         // for them to learn.
       } else {
         nextAction();
       }
     })
     .fail(function (xhr) {
       // Fail silently for now.
     });
  }

  $(function () {
    var linkEl = link().hide().appendTo('#body');
    var actionEl = actionArea().hide().appendTo('#body');
    var storiesEl = stories().hide().appendTo('#body');
    $('#confirm').click(function () {confirm();});
    var keyConfirm = function () {
      var button = $('#confirm');
      button.addClass('pressed');
      setTimeout(function () {
         button.removeClass('pressed');
         confirm();
      }, 200);
    }
    $(document).bind('keyup', 'return', keyConfirm);
    $(document).bind('keyup', 'space', keyConfirm);
    nextAction = function () {
      if (review.length > 0) {
        doReview(review.pop());
      } else if (learn.length > 0) {
        if (!V.loggedIn()) {
          var queue = V.getLocal('learnQueue', []);
        }
        if (!V.loggedIn() && queue.length == 2 && firstLink) {
          doReview(firstLink);
          tourReview();
          $('#confirm').one('click', function () {
            $('.tip').remove();
          });
        } else if (!V.loggedIn() && queue.length == 5) {
          $('#signup-invitation').append(V.signupForm()).modal({'close': false});
        } else {
          doLearn(learn.pop());
        }
      } else {
        // Clear the learning area.
        $('h1.link, #action-area, #linkword-stories').hide();
        header('Loading More Words...');
        fetchUpcoming(nextAction);
      }
    };
    var doReview = function (link) {
      $('#linkword-stories').hide();
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
      if (!V.loggedIn() && !firstLink) {
        firstLink = link;
      }
      linkEl.hide();
      updateLink(linkEl, link);
      updateStories(storiesEl, link);
      header('Study This Word');
      actionEl.show();
      $('#confirm').text('OK, Got it').show();
      confirm = function () {
        addToReview(link[0]);
        addToLearnCloud($('h1.link'), nextAction);
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

    // Start the tour for new users.
    if (!V.loggedIn() && V.getLocal('learnQueue').length < 5) {
      tourLearn();
    }
  });

  function tooltipBelow(source, target) {
    var pos = target.position();
    var width = target.outerWidth();
    target.after(source);
    var x = pos.left + (width - source.outerWidth()) / 2;
    var y = pos.top + target.outerHeight() + 10;
    source.css({'position': 'absolute', 'top': y, 'left': x});
    $('<div class="nib top"></div>').prependTo(source).css({'position': 'absolute', 'top': -14, 'left': (source.outerWidth() - 14) / 2});
    $('<a href="" class="close">x</a>').prependTo(source).click(function () {
      source.remove();
      return false;
    });
  }

  function tooltipAbove(source, target) {
    var pos = target.position();
    var width = target.outerWidth();
    target.after(source);
    var x = pos.left + (width - source.outerWidth()) / 2;
    var y = pos.top - 10 - source.outerHeight();
    source.css({'position': 'absolute', 'top': y, 'left': x});
    $('<div class="nib bottom"></div>').prependTo(source).css({'position': 'absolute', 'bottom': -14, 'left': (source.outerWidth() - 14) / 2});
    $('<a href="" class="close">x</a>').prependTo(source).click(function () {
      source.remove();
      return false;
    });
  }

  function tourLearn() {
    var tip = $('<div class="tip">'
                + '<p><img src="//s.vocabulink.com/img/wizard.png" style="float: left; margin-right: 0.75em; margin-bottom: 0.5em;">First time here? Not sure what to do? I can walk you through the basics.</p>'
                + '<p style="text-align: right"><button class="button dark">Start the Tour</button></p>'
              + '</div>').css({'max-width': '16em'
                              ,'position': 'absolute'
                              ,'top': $('h1.link').position().top
                              ,'left': 20});
    $('<a href="" class="close">x</a>').prependTo(tip).click(function () {
      tip.remove();
      return false;
    });
    tip.appendTo('body');
    $('button', tip).click(function () {
      $(this).closest('.tip').remove();
      var tip = $('<div class="tip">'
                  + '<p>This is the ' + learnLanguage + ' word you\'re going to learn. You can click the speaker icon next to it to hear its pronunciation.</p>'
                  + '<p style="text-align: right"><button class="button dark">Next: Translation</button></p>'
                + '</div>').css({'max-width': '16em'});
      $('button', tip).click(function () {
        $(this).closest('.tip').remove();
        var tip = $('<div class="tip">'
                    + '<p>This is the ' + knownLanguage + ' translation. It\'s what you\'re going to try to remember (you will be quizzed on it later).</p>'
                    + '<p style="text-align: right"><button class="button dark">Next: Linkword</button></p>'
                  + '</div>').css({'max-width': '16em'});
        $('button', tip).click(function () {
          $(this).closest('.tip').remove();
          var tip = $('<div class="tip">'
                      + '<p>And this is the "linkword". It\'s a word or phrase in ' + knownLanguage + ' that sounds a little like the ' + learnLanguage + ' word. With a story, it\'s going to link the sound of the ' + learnLanguage + ' word to its meaning.</p>'
                      + '<p style="text-align: right"><button class="button dark">Next: Stories</button></p>'
                    + '</div>').css({'max-width': '20em'});
          $('button', tip).click(function () {
            $(this).closest('.tip').remove();
            var tip = $('<div class="tip">'
                        + '<p>These are the linkword stories. Read the story and notice that it uses both the linkword and the translation. Why read the story? Because your brain remembers stories better than isolated words.</p>'
                        + '<p style="text-align: right"><button class="button dark">Next: Finish</button></p>'
                      + '</div>').css({'max-width': '20em'});
            $('button', tip).click(function () {
              $(this).closest('.tip').remove();
              var tip = $('<div class="tip">'
                          + '<p>Click this button once you think you\'ve committed this word to memory, and we\'ll move on to the next word.</p>'
                        + '</div>').css({'max-width': '20em'});
              $('#confirm').one('click', function () {
                $('.tip').remove();
              });
              tooltipBelow(tip, $('#confirm'));
            });
            tooltipAbove(tip, $('.linkword-story-container:first'));
            return false;
          });
          tooltipBelow(tip, $('h1 .link'));
          return false;
        });
        tooltipBelow(tip, $('h1.link .familiar'));
        return false;
      });
      tooltipBelow(tip, $('h1.link .foreign'));
    });
  }

  function tourReview() {
    $('.tip').remove();
    var tip = $('<div class="tip">'
                + '<p><img src="//s.vocabulink.com/img/wizard.png" style="float: left; margin-right: 0.75em; margin-bottom: 0.5em;">Every time we show you a word, we keep track of that word for you and occasionally remind you to review it.</p>'
                + '<p style="text-align: right"><button class="button dark">Show Me How</button></p>'
              + '</div>').css({'max-width': '16em'
                              ,'position': 'absolute'
                              ,'top': $('h1.link').position().top
                              ,'left': 20});
    $('<a href="" class="close">x</a>').prependTo(tip).click(function () {
      tip.remove();
      return false;
    });
    tip.appendTo('body');
    $('button', tip).click(function () {
      $(this).closest('.tip').remove();
      var tip = $('<div class="tip">'
                  + '<p>Do you remember what this word means? If you\'re having trouble, try remembering what the word sounds like (the linkword) and the story you read. Don\'t worry if you don\'t remember, but don\'t give up immediately.</p>'
                  + '<p style="text-align: right"><button class="button dark">Next: The Reveal</button></p>'
                + '</div>').css({'max-width': '20em'});
      $('button', tip).click(function () {
        $(this).closest('.tip').remove();
        var tip = $('<div class="tip">'
                    + '<p>Now, whether you remembered or not, it\'s time to reveal the answer. Click "Reveal Answer" or press the Enter key to continue.</p>'
                  + '</div>').css({'max-width': '20em'});
        $('#confirm').one('click', function () {
          var tip = $('<div class="tip">'
                      + '<p>Now that the answer has been revealed, how well did you remember it? If you remembered clearly and immediately, choose "perfect". If you had to struggle to remember, choose "barely". If your couldn\'t remember anything at all about the word, choose "blank".</p>'
                    + '</div>').css({'max-width': '25em'});
          tooltipBelow(tip, $('#grades'));
        });
        tooltipBelow(tip, $('#confirm'));
      });
      tooltipBelow(tip, $('h1.link .foreign'));
    });
  }

  function addToLearnCloud(linkEl, nextAction) {
    var learnCloud = $('#learn-cloud');
    if (learnCloud.length === 0) {
      var headBar = $('#head-bar');
      learnCloud = $(
        '<div id="learn-cloud">'
        + '<h3>Words Learned</h3>'
        + '<ul></ul>'
      + '</div>').hide().appendTo(headBar).slideDown('slow');
      learnCloud.css({'top': headBar.offset().top + headBar.outerHeight()
                     ,'left': headBar.offset().left + headBar.outerWidth() - learnCloud.outerWidth() - 3});
    }
    var foreignWord = $('.foreign-word', linkEl);
    // Put the word into place to figure out its position.
    var word = $('<li></li>').text(foreignWord.text()).css({'opacity': 0}).appendTo($('ul', learnCloud));
    var start = foreignWord.offset();
    var end = word.offset();
    // Fly the word from its element into the cloud.
    var flier = foreignWord.clone().css({'position': 'absolute'
                                        ,'left': foreignWord.position().left
                                        ,'top': foreignWord.position().top}).insertAfter(foreignWord);
    foreignWord.css({'opacity': 0});
    // Fly into position.
    flier.animate({'left': '+=' + Math.round(end.left - start.left)
                  ,'top': '+=' + Math.round(end.top - start.top)
                  ,'font-size': '6pt'
                  }, 'slow', 'swing', function () {
                    // Now, pop into place.
                    flier.remove();
                    word.css({'opacity': 1
                             ,'font-size': '6pt'}).animate({'font-size': '12pt'}, 'slow', 'easeOutBack');
                    foreignWord.css({'opacity': 1});
                    nextAction();
                  });
  }
})(jQuery);

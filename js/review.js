// Copyright 2008, 2009, 2011, 2012 Chris Forno
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

var links = [];
var total = 0;
var correct = 0;
var wrong = 0;

function grade(g) {
  var h1 = $('#review-area h1');
  if (!h1.attr('stop')) {
    return;
  }
  $.ajax('/review/' + h1.attr('linkno')
        ,{'type': 'POST'
         ,'data': {'grade': g
                  ,'time': parseInt(h1.attr('stop'), 10) - parseInt(h1.attr('start'), 10)}})
   .fail(function () {V.toastError('Failed to record grade.');});
  if (g >= 0.5) {
    correct++;
    $('#review-progress div').animate({'width': correct/total * 100 + '%'}, 'fast');
    // Update the "X links to review" in the header.
    V.incrLinksToReview(-1);
  } else {
    wrong++;
  }
  $('#grades').hide();
  $('#recall-area h2').css('visibility', 'hidden');
  $('#reveal').show();
  if (links.length) {
    reviewLink(links.shift());
  } else {
    if (correct < total) {
      // In case the learner didn't get all links correct, there are some
      // stragglers. Now's the time to review them.
      // Note that this isn't guaranteed to get just the stragglers. There's a
      // chance that other links came due in the meantime. The fix needs to be
      // completed on the server side.
      $('#review-area').mask('Loading words...');
      $.get('/review/next?n=' + (total - correct))
       .done(function (links_) {
         $('#review-area').unmask();
         $('#recall-area').show();
         links = links_;
         reviewLink(links.shift());
       })
       .fail(function () {V.toastError("Failed to retrieve links to review.");});
    } else {
      $('#recall-area').hide();
      // TODO: Make this more impressive and fun to work towards.
      $('#review-area').empty().append('<h1>Congratulations, you\'re done!</h1>');
    }
  }
}

function revealAnswer() {
  var h1 = $('#review-area h1');
  if (h1.attr('stop')) {
    return;
  }
  h1.attr('stop', Date.now());
  h1.addClass(h1.attr('type'));
  h1.find('.link').text(h1.find('.link').attr('linkword'));
  $('.familiar', h1).text(h1.find('.familiar').attr('familiar'));
  h1.find('.caption').remove();
  V.annotateLink(h1);
  $('#reveal').hide();
  $('#recall-area h2').css('visibility', 'visible');
  $('#grades').show();
}

function reviewLink(link) {
  var h1 = $('<h1 class="link" linkno="' + link.linkNumber + '" type="' + link.linkType + '">'
             + '<span class="foreign" title="' + link.foreignLanguage + '">' + link.foreign + '</span>'
             + '<span class="link"></span>'
             + '<span class="familiar" title="' + link.familiarLanguage + '" familiar="' + link.familiar + '">?</span>'
           + '</h1>');
  h1.find('.link').attr('linkword', (link.linkword ? link.linkword : ''));
  if (link.pronunciation) {
    $('<button id="pronounce" class="button light">'
      + '<audio>'
        + '<source src="http://s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.ogg">'
        + '<source src="http://s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.mp3">'
      + '</audio>'
      + '<img src="http://s.vocabulink.com/img/icon/audio.png">'
    + '</button>').click(function () {
      $(this).find('audio')[0].play();
    }).appendTo(h1.find('.foreign'));
  }
  $('#review-area').empty().append(h1);
  V.annotateLink(h1);
  h1.attr('start', Date.now());
}

function getLinks(stats) {
  if (stats.due == 0) {
    $('#recall-area').hide();
    $('#review-area').empty().append('<h1>No links ready for review!</h1>');
  } else {
    $.get('/review/next?n=' + stats.due)
     .done(function (links_) {
       $('#review-area').unmask();
       $('#recall-area').show();
       links = links_;
       total = links.length;
       if (total > 0) {
         reviewLink(links.shift());
       }
     })
    .fail(function () {V.toastError("Failed to retrieve links to review.");});
  }
}

$(function () {
  $('#body').append('<div class="progress-bar" id="review-progress"><div style="width: 0%"></div></div>');
  $('<div id="review-area"></div>').appendTo('#body').mask('Loading words...');
  $('<div id="recall-area" style="display: none"><h2 style="visibility: hidden;">How well did you remember?</h2><button id="reveal" class="faint-gradient-button green" title="hotkey: enter">Reveal Answer</button></div>').appendTo('#body');
  $.get('/review/stats')
   .done(getLinks)
   .fail(function () {V.toastError("Failed to retrieve link stats.");});
  var grades = $('<div id="grades"></div>').hide();
  $.each(['blank', 'bad', 'wrong', 'barely', 'good', 'perfect'], function (i, text) {
    var button = $('<button class="grade' + i + '" grade="' + (i / 5) + '" title="hotkey: ' + (i + 1) + '"><b></b><br>' + text + '</button>');
    button.click(function () {grade(i / 5);});
    $(document).bind('keyup', (i + 1).toString(), function () {
      button.addClass('pressed');
      setTimeout(function () {
        button.removeClass('pressed');
        grade(i / 5);
      }, 250);
    })
    grades.append(button);
  });
  $('#recall-area').append(grades);
  $('#reveal').click(revealAnswer);
  $(document).bind('keyup', 'return', revealAnswer);
});

})(jQuery);

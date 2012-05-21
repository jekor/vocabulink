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

function progressBar(total) {
  var barDiv = $('<div class="progress-bar" id="review-progress"><div style="width: 0%"></div></div>');
  var counter = 0;
  return [barDiv, function () {
    $('div', barDiv).animate({'width': (++counter)/total * 100 + '%'}, 'fast');
  }];
}

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
    // Update the "X links to review" in the header.
    V.incrLinksToReview(-1);
  }
  $('#grades').hide();
  $('#recall-area h2').css('visibility', 'hidden');
  $('#reveal').show();
}

function revealAnswer() {
  var h1 = $('#review-area h1');
  if (h1.attr('stop') || !h1.hasClass('link')) {
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

function revealLink(link) {
  var h1 = $(
    '<h1 class="link">'
    + '<span class="foreign"></span>'
    + '<span class="link"></span>'
    + '<span class="familiar">?</span>'
  + '</h1>');
  h1.attr('linkno', link.linkNumber).attr('type', link.linkType);
  h1.find('.foreign').attr('title', link.foreignLanguage).text(link.foreign);
  h1.find('.familiar').attr('title', link.familiarLanguage).attr('familiar', link.familiar);
  h1.find('.link').attr('linkword', (link.linkword ? link.linkword : ''));
  if (link.pronunciation) {
    var button = $(
      '<button id="pronounce" class="button light">'
      + '<audio></audio>'
      + '<img src="http://s.vocabulink.com/img/icon/audio.png">'
    + '</button>');
    button.find('audio')
      .append($('<source></source>').attr('src', 'http://s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.ogg'))
      .append($('<source></source>').attr('src', 'http://s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.mp3'));
    button.click(function () {
      $(this).find('audio')[0].play();
    }).appendTo(h1.find('.foreign'));
  }
  $('#review-area').empty().append(h1);
  V.annotateLink(h1);
  h1.attr('start', Date.now());
}

$(function () {
  $('<div id="review-area"></div>').appendTo('#body').mask('Loading words...');
  $('<div id="recall-area" style="display: none"><h2 style="visibility: hidden;">How well did you remember?</h2><button id="reveal" class="faint-gradient-button green" title="hotkey: enter">Reveal Answer</button></div>').appendTo('#body');
  $.get('/review/next')
   .done(function (links) {
     $('#review-area').unmask();
     $('#recall-area').show();
     var progress = progressBar(links.length);
     var progressDiv = progress[0];
     var progressIncr = progress[1];
     $('#body').prepend(progressDiv.attr('id', 'review-progress'));

     var reviewNext = function () {
       if (links.length > 0) {
         revealLink(links.shift());
       } else {
         $('#recall-area').hide();
         // TODO: Make this more impressive and fun to work towards.
         $('#review-area').empty().append('<h1>Congratulations, you\'re done!</h1>');
       }
     };

     var grade_ = function (score) {
       grade(score);
       progressIncr();
       reviewNext();
     };

     var grades = $('<div id="grades"></div>').hide();
     $.each(['blank', 'wrong', 'almost', 'barely', 'good', 'perfect'], function (i, text) {
       var button = $('<button><b></b><br></button>');
       button.addClass('grade' + i).attr('grade', i / 5).attr('title', 'hotkey: ' + (i + 1));
       button.append(text);
       button.click(function () {grade_(i / 5);});
       $(document).bind('keyup', (i + 1).toString(), function () {
         if ($('#grades:visible').length > 0) {
           button.addClass('pressed');
           setTimeout(function () {
             button.removeClass('pressed');
             grade_(i / 5);
           }, 250);
         }
       })
       grades.append(button);
     });
     $('#recall-area').append(grades);

     reviewNext();
   })
   .fail(function () {V.toastError("Failed to retrieve links to review.");});
  $('#reveal').click(revealAnswer);
  $(document).bind('keyup', 'return', revealAnswer);
});

})(jQuery);

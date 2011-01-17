// Copyright 2008, 2009, 2011 Chris Forno
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

function revealAnswer(link, startTime) {
  var stopTime = new Date();
  link.hide();
  $('#full-link').show();
  V.annotateLink($('#full-link'));
  var recallTime = stopTime.getTime() - startTime.getTime();
  $('#recall-time').val(recallTime);
  $('#recall-buttons').show();
}

function updateCountdown() {
  updateCountdown.seconds -= 1;
  if (updateCountdown.seconds < 1) {
    $(updateCountdown.elem).text('now');
    clearInterval(updateCounter.timer);
  } else {
    $(updateCountdown.elem).text('in ' + formatSeconds(updateCountdown.seconds));
  }
}

function formatSeconds(seconds) {
  units = {day: 86400, hour: 3600, minute: 60};
  counts = {day: 0, hour: 0, minute: 0, second: 0};
  output = '';
  for (var unit in units) {
    if (seconds > units[unit]) {
      counts[unit] = Math.floor(seconds / units[unit]);
      seconds %= units[unit];
    }
  }
  counts.second = seconds;
  printing = false;
  for (var unit in counts) {
    if (counts[unit] > 0) printing = true;
    if (printing) {
      output += ' ' + counts[unit] + ' ' + unit + (counts[unit] == 1 ? '' : 's');
    }
  }
  return output;
}

$(function () {
  if ($('#review-link').length) {
    V.annotateLink($('#review-link'));
    var startTime = new Date();
    $('#review-link a').one('click', function () {
      revealAnswer($(this).parent(), startTime);
    });
    $(document).bind('keyup', function (e) {
      if (e.keyCode == 32) { // spacebar
        revealAnswer($('#review-link'), startTime);
        return false;
      }
      var zero     = 48; // key code for the '0' key
      var pad_zero = 96; // We also want to support the number pad.
      if ($('#recall-buttons:visible').length &&
          ((e.keyCode >= zero && e.keyCode <= zero + 5) ||
                        e.keyCode >= pad_zero && e.keyCode <= pad_zero + 5)) {
        var button_num = e.keyCode - (e.keyCode < pad_zero ? zero : pad_zero);
        $('#recall-buttons button')[button_num].click();
        return false;
      }
      return true;
    });
  }
  $('#countdown').each(function () {
    updateCountdown.seconds = $(this).find('.seconds').text();
    updateCountdown.elem = $(this);
    updateCountdown.timer = setInterval(updateCountdown, 1000);
  });
});

})(jQuery);
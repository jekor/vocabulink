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

function getReviewStats(startTime) {
  var stopTime = new Date();
  var recallTime = stopTime.getTime() - startTime.getTime();
  $('#recall-time').val(recallTime);
  $('#recall-buttons').show();
}

function drawReview(link) {
  var startTime = new Date();
  var g = drawLinkReview(link);
  var revealed = false;
  var reveal = function () {
    revealed = true;
    g.graph.remove();
    drawLink(link);
    getReviewStats(startTime);
  };
  g.node.click(reveal);
  $(document).bind('keyup', function (e) {
    if (e.keyCode == 32) { // spacebar
      reveal(startTime);
      return false;
    }
    var zero     = 48; // key code for the '0' key
    var pad_zero = 96; // We also want to support the number pad.
    if (revealed && ((e.keyCode >= zero && e.keyCode <= zero + 5) ||
                      e.keyCode >= pad_zero && e.keyCode <= pad_zero + 5)) {
      var button_num = e.keyCode - (e.keyCode < pad_zero ? zero : pad_zero);
      $('#recall-buttons button')[button_num].click();
      return false;
    }
    return true;
  });
}

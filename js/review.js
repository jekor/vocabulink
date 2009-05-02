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

function drawReview(link) {
  var startTime = new Date();
  var g = drawLinkReview(link);
  var revealed = false;
  var reveal = function() {
    revealed = true;
    g.graph.remove();
    drawLink(link);
    getReviewStats(startTime);
  }
  g.node.click(reveal);
  connect(document, 'onkeyup', function(e) {
      var k = e.key();
      if (k.string == 'KEY_SPACEBAR') {
        stop();
        reveal(startTime);
      }
      var zero = 48; // key code for the '0' key
      var pad_zero = 96; // We also want to support the number pad.
      if (revealed && ((k.code >= zero && k.code <= zero + 5) ||
                       (k.code >= pad_zero && k.code <= pad_zero + 5))) {
        stop();
        var buttons = $$('#recall-buttons button');
        var button_num = k.code - (k.code < pad_zero ? zero : pad_zero);
        buttons[button_num].click();
      }
  });
}

function getReviewStats(startTime) {
  var stopTime = new Date();
  var recallTime = stopTime.getTime() - startTime.getTime();
  setNodeAttribute($('recall-time'), 'value', recallTime);
  showElement($('recall-buttons'));
  stop();
}

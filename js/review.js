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

// This draws a link with the destination obscured (for reviews).
function drawLinkReview(g, link, callback) {
  // We want to give away as little as possible about the link.
  // The idea is that eventually we'll support hinting. When a member asks for
  // a hint, we might show the color of the link. The next hint might be the
  // link word (if one exists), etc.
  // At first, I thought I could repurpose drawLink() or make it more general.
  // But after some review it has less in common with the needs of this
  // function than I thought.
  var l = g.graph.path({'stroke': '#000', 'stroke-width': 3}).moveTo(g.width*0.3, g.height/2).lineTo(g.width*0.7, g.height/2);
  var origNode = graphNode(g, link.orig, {'x': g.width*0.3, 'y': g.height/2}, '#000', '#000', '#DFDFDF');
  var destNode = graphNode(g, '?', {'x': g.width*0.7, 'y': g.height/2}, '#000', '#000', '#DFDFDF');
  var mouseIn  = function (node) {
    node.ellipse.animate({'fill': node.hovercolor}, 250);
    document.body.style.cursor = 'pointer';
  };
  var mouseOut = function (node) {
    node.ellipse.animate({'fill': '#FFF'}, 250);
    document.body.style.cursor = 'auto';
  };
  origNode.mouseover(mouseIn.curry(origNode));
  origNode.mouseout(mouseOut.curry(origNode));
  destNode.mouseover(mouseIn.curry(destNode));
  destNode.mouseout(mouseOut.curry(destNode));
  return {'graph': g.graph, 'node': destNode};
  // var reveal = function () {g.graph.remove(); drawLink(g, link);};
  // destNode.click(callback);
  // return reveal;
}

function drawReview(block, link) {
  var startTime = new Date();
  var g = createGraph_(block);
  var h = drawLinkReview(g, link);
  var revealed = false;
  var reveal = function () {
    revealed = true;
    getReviewStats(startTime);
    g = createGraph_(g.node);
    drawLink(g, link);
  };
  h.node.click(reveal);
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

function drawLinkReview_() {
  var orig = $(this).find('.orig').text();
  var dest = $(this).find('.dest').text();
  var label = '';
  var colors = linkColors['association'];
  if ($(this).hasClass('cognate')) {
    colors = linkColors['cognate'];
  }
  if ($(this).hasClass('linkword')) {
    label = $(this).find('.linkword').text();
    colors = linkColors['linkword'];
  }
  drawReview(this, {'orig': orig, 'dest': dest, 'label': label,
                    'color': colors[0], 'bgcolor': colors[1]});
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

$(document).ready(function () {
  $('h1.link.review').each(drawLinkReview_);
  $('#countdown').each(function () {
    updateCountdown.seconds = $(this).find('.seconds').text();
    updateCountdown.elem = $(this);
    updateCountdown.timer = setInterval(updateCountdown, 1000);
  });
});

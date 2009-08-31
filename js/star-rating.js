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

// This is for star-ratings. It currently only happens on link pages.

$(document).ready(function() {
  $('.rating.enabled').each(setupRating);
});

function setupRating() {
  var starsBase = $(this).find('.stars-base:first');
  var stars = starsBase.find('.stars:first');
  var originalWidth = stars.css('width');
  var originalPosition = stars.css('background-position');
  var details = {'width': 100, 'numStars': 5, 'numColors': 5, 'spriteHeight': 22};
  starsBase.mouseenter(beginRating.curry(stars, starsBase, details));
  starsBase.mouseleave(endRating.curry(stars, originalWidth, originalPosition));
}

function beginRating(stars, starsBase, details, e) {
  var x = Math.round($(e.target).offset().left);
  $(this).mousemove(trackRating.curry(x, stars, details));
  $(this).click(recordRating.curry(x, starsBase, details));
}

function endRating(stars, originalWidth, originalPosition, e) {
  $(this).unbind('mousemove click');
  stars.css('width', originalWidth).css('background-position', originalPosition);
}

function trackRating(x, stars, details, e) {
  var percentage = (e.pageX - x) / details.width;
  var pixels = Math.ceil(percentage * details.numStars) *
               (details.width / details.numStars);
  // Calculating the color is a bit of a challenge. This is because we have 4
  // colors but 5 stars. What we do is divide up our percentage into the 4
  // stars. We do this because the first star is always filled. If we didn't
  // any rating above an absolute 0% wouldn't get the first star color.
  var color = Math.ceil(percentage * details.numStars);
  var position = (-1 * details.spriteHeight * (details.numColors)) +
                 (color - 1) * details.spriteHeight;
  stars.css('width', pixels + 'px').css('background-position', 'left ' + position + 'px');  
}

function recordRating(x, starsBase, details, e) {
  starsBase.unbind();
  var percentage = (e.pageX - x) / details.width;
  // Again, the 5-star system is a little tricky. If we count a 1-star vote as
  // 20%, that would mean that the member liked it at least somewhat. But a
  // 1-star vote is the lowest you can vote, and hence is logically 0%. So what
  // we'll do is divide up the percentage among the rest of the 4 stars.
  // 1 star = 0%, 2 stars = 25%, 3 stars = 50%, and so on.
  var rating = (Math.ceil(percentage * details.numStars) - 1) / 4;
  // var url = $(e.target).find('form:first').attr('action');
  var url = $(e.target).attr('action');
  $(e.target).unbind();
  var p = starsBase.parent().find('p:first');
  p.text("Rating...");
  $.ajax({'type': 'POST', 'url': url,
          'data': {'rating': rating},
          'success': function() {
            p.text("Thanks!");
          },
          'error':   function() { p.text("Failed to rate!"); }});
}

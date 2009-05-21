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

// This is for site-wide dynamic interaction with members.

connect(window, 'onload', setup);

function postXHR(url, postVars) {
  return doXHR(url, {'method': 'POST',
                     'headers': {'Content-Type': 'application/x-www-form-urlencoded'},
                     'sendContent': queryString(postVars)});
}

function setup() {
  setupRatings();
}

function setupRatings() {
  var ratings = $$('.rating.enabled');
  map(setupRating, ratings);
}

function setupRating(rating) {
  var starsBase = getFirstElementByTagAndClassName(null, 'stars-base', rating);
  var stars = getFirstElementByTagAndClassName(null, 'stars', starsBase);
  var originalWidth = getStyle(stars, 'width');
  var originalPosition = getStyle(stars, 'background-position');
  var details = {'width': 100, 'numStars': 5, 'numColors': 5, 'spriteHeight': 22};
  connect(starsBase, 'onmouseenter', partial(beginRating, stars, details));
  connect(starsBase, 'onmouseleave', partial(endRating, stars, originalWidth, originalPosition));
}

function beginRating(stars, details, e) {
  var position = getElementPosition(this);
  var x = Math.round(position.x);
  connect(this, 'onmousemove', partial(trackRating, x, stars, details));
  connect(this, 'onclick', partial(recordRating, x, details));
}

function endRating(stars, originalWidth, originalPosition, e) {
  disconnectAll(this, 'onmousemove', 'onclick');
  setStyle(stars, {'width': originalWidth,
                   'background-position': originalPosition});
}

function trackRating(x, stars, details, e) {
  var percentage = (e.mouse().page.x - x) / details.width;
  var pixels = Math.ceil(percentage * details.numStars) *
               (details.width / details.numStars);
  // Calculating the color is a bit of a challenge. This is because we have 4
  // colors but 5 stars. What we do is divide up our percentage into the 4
  // stars. We do this because the first star is always filled. If we didn't
  // any rating above an absolute 0% wouldn't get the first star color.
  var color = Math.ceil(percentage * details.numStars);
  var position = (-1 * details.spriteHeight * (details.numColors)) +
                 (color - 1) * details.spriteHeight;
  setStyle(stars, {'width': pixels + 'px',
                   'background-position': 'left ' + position + 'px'});
}

function recordRating(x, details, e) {
  var percentage = (e.mouse().page.x - x) / details.width;
  // Again, the 5-star system is a little tricky. If we count a 1-star vote as
  // 20%, that would mean that the member liked it at least somewhat. But a
  // 1-star vote is the lowest you can vote, and hence is logically 0%. So what
  // we'll do is divide up the percentage among the rest of the 4 stars.
  // 1 star = 0%, 2 stars = 25%, 3 stars = 50%, and so on.
  var rating = (Math.ceil(percentage * details.numStars) - 1) / 4;
  var url = getNodeAttribute(getFirstElementByTagAndClassName('form', null, this), 'action');
  disconnectAll(this);
  var p = getFirstElementByTagAndClassName('p', null, getFirstParentByTagAndClassName(this));
  var statusP = swapDOM(p, P(null, 'Rating...'));
  var d = postXHR(url, {'rating': rating});
  d.addCallbacks(function(r) {ratingSuccess(statusP);},
                 function(r) {ratingFailure(statusP);});
}

function ratingSuccess(statusP) {
  swapDOM(statusP, P(null, 'Thanks!'));
  log('success');
}

function ratingFailure(statusP) {
  swapDOM(statusP, P(null, 'Failed to rate!'));
  log('failure');
}

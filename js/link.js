// Copyright 2009, 2010 Chris Forno
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

$(document).ready(function () {
  var link = $('h1.link');
  link.find('span').each(function () {
    var word = $(this);
    var caption = $('<span class="caption">' + word.attr('title') + '</span>');
    caption.insertAfter(link);
    var x = word.offset().left + ((word.outerWidth() - caption.width()) / 2);
    if (word.hasClass('.orig') || word.hasClass('.dest')) {
      var y = word.offset().top + word.outerHeight() + 8;
    } else {
      var y = word.offset().top + word.height() + 8;
    }
    caption.css({'position': 'absolute', 'left': x, 'top': y});
  });

  // "add to review"
  $('#link-op-review.enabled').click(function () {
    var op = $(this);
    op.mask("Adding...");
    var linkNum = window.location.pathname.split('/').pop();
    $.postJSON('/review/' + linkNum + '/add', null, function (successful, data) {
      op.unmask();
      op.removeClass("enabled").addClass("disabled");
      if (successful) {
        op.text("now reviewing");
      } else {
        op.addClass("failed");
        op.text("Failed!");
      }
    });
  });

  // "delete link"
  $('#link-op-delete.enabled').click(function () {
    var op = $(this);
    op.mask("Deleting...");
    var linkNum = window.location.pathname.split('/').pop();
    $.postJSON('/link/' + linkNum + '/delete', null, function (successful, data) {
      op.unmask();
      op.removeClass("enabled").addClass("disabled");
      if (successful) {
        op.text("deleted");
      } else {
        op.addClass("failed");
        op.text("Failed!");
      }
    });
  });
});

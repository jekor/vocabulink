// Copyright 2009, 2010, 2011 Chris Forno
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

function annotateLink(link) {
  link.children('.orig, .link, .dest').each(function () {
    var word = $(this);
    var caption = $('<span class="caption">' + word.attr('title') + '</span>');
    // We have to calculate these before we add content to them and screw up
    // the dimensions.
    var width = word.outerWidth();
    if (word.hasClass('.orig') || word.hasClass('.dest')) {
      var y = word.outerHeight() + 4;
    } else {
      var y = word.height() + 8;
    }
    caption.appendTo(word);
    var x = (width - caption.width()) / 2;
    caption.css({'position': 'absolute', 'left': x, 'top': y});
  });
}

function linkNumber() {
    return window.location.pathname.split('/').pop();
}

$(document).ready(function () {
  annotateLink($('h1.link:visible'));

  $('#pronounce').click(function () {
    $(this).find('audio')[0].play();
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

  $('#link-op-add-pronunciation.enabled').click(function () {
    var box = $('<div class="operation-box">'
              + '</div>').hide().appendTo('#link-ops');
    var uploader = new qq.FileUploader({
      'element': box[0],
      'action': '/link/' + linkNumber() + '/pronunciation',
      'allowedExtensions': ['flac', 'wav', 'ogg', 'mp3'],
      'onComplete': function (id, fileName, responseJSON) {
        box.slideUp('fast');
        box.remove();
        location.reload();
      }
    });
    box.slideDown('fast');
  });

  $('#link-op-delete-pronunciation.enabled').click(function () {
    var button = $(this);
    button.text("deleting...");
    $.ajax({'type': 'DELETE'
           ,'url': '/link/' + linkNumber() + '/pronunciation'
           ,'success': function () {
              button.text("Deleted.");
            }
           ,'error': function () {
              button.text("Failed to delete.");
            }
           });
  });

  // "delete link"
  $('#link-op-delete.enabled').click(function () {
    var op = $(this);
    op.mask("Deleting...");
    $.postJSON('/link/' + linkNumber() + '/delete', null, function (successful, data) {
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

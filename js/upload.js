// Copyright 2009, 2011 Chris Forno
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

// This is kludgy. It's a hold-over from pre-jQuery days. This will only work
// on the "Create Link Pack" page.

(function ($) {

function submitFile(fileInput, fileButton) {
  fileInput.css('background',
                "url('http://s.vocabulink.com/img/wait-bar.gif') no-repeat center center");
  fileButton.attr('disabled', null);
}

function fileSubmitted(fileInput, fileButton, file, response) {
  if (response.substr(0, 11) === '/pack/image') {
    fileInput.css('background', 'none');
    fileInput.val(response.substr(12));
    fileInput.removeAttr('disabled');
    fileInput.attr('readonly', null);
  } else {
    alert('Error uploading file.');
  }
}

$(function () {
  var fileInput = $('.upload-file');
  var fileButton = $('#upload-file-button');
  new AjaxUpload('upload-file-button', {'action': '/pack/image',
                                        'onSubmit': function () {submitFile(fileInput, fileButton);},
                                        'onComplete': function () {fileSubmitted(fileInput, fileButton);}});
  fileButton.removeAttr('disabled');
});

})(jQuery);
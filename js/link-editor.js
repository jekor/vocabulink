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

// This is for the original link creation page.
function showLinkEditor() {
  var linkType = this.options[this.selectedIndex].text.replace(/ /g, '-');
  $('fieldset').hide();
  $('#' + linkType).show();
}

function saveChanges() {
  var button = $(this);
  var linkDetailsBox = $('.link-details');
  var body = $.trim(linkDetailsBox.find('textarea').val());
  if (body === '') {
    alert('Please enter some text.');
    return false;
  }
  button.text('Saving...');
  button.attr('disabled', 'disabled');
  var removeOverlay = overlay(linkDetailsBox);
  // We get the link number from the URL.
  var linkNumber = parseInt(window.location.pathname.split('/').pop(), 10);
  $.ajax({'type': 'POST', 'url': '/link/' + linkNumber + '/story',
          'contentType': 'text/plain',
          'data': body,
          'dataType': 'html',
          'success': function (data) {
            button.text('Saved!');
            removeOverlay();
            $('#link-details').html(data).show();
            linkDetailsBox.find('.markItUp').remove();
          },
          'error':   function (data) {
            alert('Error saving link story.');
            button.text('Save Changes');
            button.attr('disabled', null);
            removeOverlay();
          }});
  return false;
}

// This is for editing in-place on an already-created link page.
function editLink() {
  $(this).unbind('click');
  $(this).click(saveChanges);
  $(this).text('Save Changes');
  var linkDetails = $('#link-details');
  var box = linkDetails.parent().find('textarea:first');
  box.show();
  linkDetails.hide();
  box.markItUp(mySettings);
  return false;
}

$(document).ready(function () {
  var linkTypeSelector = $('select[name=fval4]:first');
  linkTypeSelector.change(showLinkEditor);
  linkTypeSelector.change();
  // Setup the link word editor right away.
  $('#link-word textarea').markItUp(mySettings);
  $('#link-edit').click(editLink);
});

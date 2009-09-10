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

// Allow for in-page editing and administrative tasks.

function connectButton() {
  var classes = $(this).attr('class').split(' ');
  $(this).click(function () {
    $('#' + classes[1]).show();
    $(this).hide();
  });
}

$(document).ready(function () {
  $('button.reveal').each(connectButton);
});

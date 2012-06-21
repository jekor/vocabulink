// Copyright 2008, 2009, 2010, 2011, 2012 Chris Forno
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

(function ($) {

var changeLinkType = function () {
  var newType = $(this).val();
  $(this).parents('h1').removeClass('linkword').removeClass('soundalike').removeClass('association')
         .addClass(newType);
  var linkword = $(this).parent().find('input');
  if (newType === 'linkword') {
    linkword.prop('required', null).css('visibility', 'visible');
  } else {
    linkword.removeAttr('required').css('visibility', 'hidden');
  }
};

$(function () {
  $('h1.link.edit .link select').change(changeLinkType).change();
  $('#body form').minform();
});

})(jQuery);

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

var V = V || {};

(function ($) {

// Crockford's prototypal inheritance operator
V.object = function (o) {
  function F() {}
  F.prototype = o;
  return new F();
};

// This returns a component of the user's authtoken cookie or null if the
// cookie does not exist or the component is not found.
function authTokenPart(key) {
  var authToken = $.cookie('auth');
  if (authToken) {
    var match = authToken.match(new RegExp(key + '=([^&]+)'));
    if (match) {
      return match[1];
    } else {
      return null;
    }
  } else {
    return null;
  }
}

// Read the member's name from the authtoken cookie. If the member's name
// cannot be found (likely because the client is not authenticated), this
// returns null.
// We don't worry about cookie tampering. Authentication verification is
// handled by the server.
V.memberName = function () {
  return authTokenPart('name');
};

V.memberGravatar = function () {
  var gravHash = authTokenPart('grav');
  if (gravHash) {
    return 'http://www.gravatar.com/avatar.php?gravatar_id=' + gravHash + '&size=60&default=wavatar';
  } else {
    return null;
  }
};

function loginPopup() {
  var headBar = $('#head-bar');
  $('<div id="login-popup">' +
      '<a class="close-button"><span>×</span></a>' +
      '<form action="/member/login" method="post">' +
        '<table>' +
          '<tbody>' +
            '<tr><th><label>Username:</label></th><td><input type="text" name="fval[0]"/></td></tr>' +
            '<tr><th><label>Password:</label></th><td><input type="password" name="fval[1]"/></td></tr>' +
          '</tbody>' +
        '</table>' +
        '<input type="submit" value="Login"/>' +
      '</form>' +
    '</div>').insertAfter(headBar)
    .css('top', headBar.offset().top + headBar.outerHeight())
    .css('left', headBar.offset().left + headBar.outerWidth() - $('#login-popup').outerWidth() - 3)
    .find('.close-button').click(function () {
      $(this).parent().remove();
    });
}

// initialization for every page
$(function () {
  Functional.install();
  try {
    var pageTracker = _gat._getTracker("UA-73938-2");
    pageTracker._trackPageview();
  } catch(err) {}
  $('#login-button').click(function () {
    if ($('#login-popup').length) {
      $('#login-popup').remove();
    } else {
      loginPopup();
    }
  });
});

})(jQuery);
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

if (!window.console) {
  window.console = {
    log: function () {}
  };
}

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

V.loggedIn = function () {
  return V.memberName() !== null;
};

V.memberGravatar = function () {
  var gravHash = authTokenPart('grav');
  if (gravHash) {
    return 'http://www.gravatar.com/avatar.php?gravatar_id=' + gravHash + '&size=60&default=wavatar';
  } else {
    return null;
  }
};

function toastNotice(msg) {
  $().toastmessage('showNoticeToast', msg);
}

function toastSuccess(msg) {
  $().toastmessage('showSuccessToast', msg);
}

function toastError(msg) {
  $().toastmessage('showErrorToast', msg);
}

function toastWarning(msg) {
  $().toastmessage('showWarningToast', msg);
}

// TODO: Factor out common code between login and signup popups.
V.loginPopup = function () {
  var headBar = $('#head-bar');
  var popup = $(
    '<form id="login-popup" action="/member/login" method="post">'
    + '<table>'
      + '<tr><td><label for="login-username">Username:</label></td><td><input id="login-username" name="username" required autofocus></td></tr>'
      + '<tr><td><label for="login-password">Password:</label></td><td><input id="login-password" type="password" name="password" required></td></tr>'
    + '</table>'
    + '<input type="submit" value="Login" class="dark">'
    + '<button class="cancel">cancel</button>'
  + '</form>').appendTo(headBar);
  popup.css('top', headBar.offset().top + headBar.outerHeight())
       .css('left', headBar.offset().left + headBar.outerWidth() - $('#login-popup').outerWidth() - 3)
       .find('.cancel').click(function () {
         $(this).parent().remove();
       });
  popup.minform();
  popup.submit(function (e) {
    popup.mask('Logging in...');
    e.preventDefault();
    $.post($(this).attr('action'), $(this).serialize())
      .done(function () {location.reload();})
      .fail(function (xhr) {
              popup.unmask();
              toastError(xhr.responseText);
              popup.find('input[name=password]').val('').focus();
            });
    return false;
  });
};

V.signupPopup = function() {
  var headBar = $('#head-bar');
  var popup = $(
    '<form id="signup-popup" action="/member/signup" method="post">'
    + '<h2>Sign Up for Free</h2>'
    + '<table>'
      + '<tr><td><label for="signup-username">Username:</label></td><td><input id="signup-username" name="username" required autofocus minlength="3" maxlength="32"></td><td></td></tr>'
      + '<tr><td><label for="signup-email">Email:</label></td><td><input id="signup-email" type="email" name="email" required></td><td></td></tr>'
      + '<tr><td><label for="signup-password">Password:</label></td><td><input id="signup-password" type="password" name="password" required></td><td></td></tr>'
      + '<tr><td colspan="2"><label for="signup-terms">I agree to the <a href="/terms-of-use" target="_blank">Terms of Use</a>.</label><input id="signup-terms" name="terms" type="checkbox" required></td></tr>'
      + '<tr><td></td><td><input type="submit" value="Signup" class="dark"></td></tr>'
    + '</table>'
  + '</form>').appendTo(headBar);
  popup.css('top', headBar.offset().top + headBar.outerHeight())
       .css('left', headBar.offset().left + headBar.outerWidth() - $('#signup-popup').outerWidth() - 3)
       .find('.cancel').click(function () {
         $(this).parent().remove();
       });
  popup.minform();
  $('#signup-username').change(function () {
    var username = $(this).val();
    $.get('http://www.vocabulink.com/member/' + username + '/available')
     .done(function (available) {
       if (available) {
         $('#signup-username').parent().parent().find('td:last-child').empty().append('<img alt="✓" title="This username is available." src="http://s.vocabulink.com/img/icon/accept.png">');
       } else {
         $('#signup-username').parent().parent().find('td:last-child').empty().append('<img alt="!" title="This username is unavailable" src="http://s.vocabulink.com/img/icon/exclamation.png">');
       }
     });
  });
  popup.submit(function (e) {
    popup.mask('Signing up...');
    e.preventDefault();
    $.post($(this).attr('action'), $(this).serialize())
     .done(function () {location.reload();})
     .fail(function (xhr) {popup.unmask(); toastError(xhr.responseText);});
    return false;
  });
}

// initialization for every page
$(function () {
  try {
    var pageTracker = _gat._getTracker("UA-73938-2");
    pageTracker._trackPageview();
  } catch(err) {}
  $('#login-button').click(function () {
    if ($('#login-popup').length) {
      $('#login-popup').remove();
    } else {
      $('#signup-popup').remove();
      V.loginPopup();
    }
  });

  $('#signup-button').click(function () {
    if ($('#signup-popup').length) {
      $('#signup-popup').remove();
    } else {
      $('#login-popup').remove();
      V.signupPopup();
    }
  });

  // Hook up any buttons that require login to popup the login box.
  $('.login-required').click(function () {
    if (!$('#login-popup').length) {
      V.loginPopup();
    }
  });

  // Display all notices in the top center.
  $().toastmessage({'position': 'top-center'});
});

})(jQuery);

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

V.incrLinksToReview = function (by) {
  var num = parseInt($('#head .review-box strong').text(), 10) + by;
  if (num == 1) {
    $('#head .review-box').empty().append('<strong>1</strong> link to review');
  } else {
    $('#head .review-box').empty().append('<strong>' + num + '</strong> links to review');
  }
}

V.toastMessage = function (type, msg, sticky) {
  return $().toastmessage('showToast', {'text': msg
                                       ,'type': type
                                       ,'sticky': sticky
                                       ,'position': 'top-center'
                                       });
}

V.toastNotice = function (msg, sticky) {
  return V.toastMessage('notice', msg, sticky);
}

V.toastSuccess = function (msg, sticky) {
  return V.toastMessage('success', msg, sticky);
}

V.toastError = function (msg, sticky) {
  return V.toastMessage('error', msg, sticky);
}

V.toastWarning = function (msg, sticky) {
  return V.toastMessage('warning', msg, sticky);
}

function lostPasswordPopup() {
  var content = $('<div><h1>Lost Password</h1>'
          + '<form id="lost-password-form" action="/member/password/reset" method="post">'
            + '<p>Enter the email address you signed up with, and we\'ll send you a link to reset your password with.</p>'
            + '<table>'
              + '<tr><th><label>Email:</label></th><td><input type="email" name="email" required autofocus style="width: 295px"></td></tr>'
              + '<tr><td colspan="2" style="text-align: right"><input class="light" type="submit" value="Send Password Recovery" style="margin-bottom: 1em"></td></tr>'
            + '</table>'
          + '</form>'
        + '</div>').css('width', '370px');
  $.modal(content);
  var form = $('#lost-password-form');
  var modal = $('#simplemodal-container');
  form.minform()
      .submit(function (e) {
        modal.mask('Sending...');
        e.preventDefault();
        $.post($(this).attr('action'), $(this).serialize())
          .done(function () {$.modal.close(); V.toastSuccess('Password recovery instructions sent.');})
          .fail(function (xhr) {modal.unmask(); V.toastError(xhr.responseText, true);});
        return false;
      });
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
    + '<button class="cancel hyperlink">lost password?</button>'
  + '</form>').appendTo(headBar);
  popup.css('top', headBar.offset().top + headBar.outerHeight())
       .css('left', headBar.offset().left + headBar.outerWidth() - $('#login-popup').outerWidth() - 3)
       .find('.cancel').click(function () {
         $(this).parent().remove();
         lostPasswordPopup();
       });
  popup.minform();
  popup.submit(function (e) {
    popup.mask('Logging in...');
    e.preventDefault();
    $.post($(this).attr('action'), $(this).serialize())
      .done(function () {location.reload();})
      .fail(function (xhr) {
              popup.unmask();
              V.toastError(xhr.responseText);
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
    $.get('http://www.vocabulink.com/user/' + username + '/available')
     .done(function (available) {
       if (available) {
         $('#signup-username').parent().parent().find('td:last-child').empty().append('<img alt="✓" title="This username is available." src="http://s.vocabulink.com/img/icon/accept.png">');
       } else {
         $('#signup-username').parent().parent().find('td:last-child').empty().append('<img alt="!" title="This username is unavailable." src="http://s.vocabulink.com/img/icon/exclamation.png">');
       }
     });
  });
  $('#signup-email').change(function () {
    var email = $(this).val();
    $.get('http://www.vocabulink.com/email/' + email + '/available')
     .done(function (available) {
       if (available) {
         $('#signup-email').parent().parent().find('td:last-child').empty().append('<img alt="✓" title="This email address is valid and available." src="http://s.vocabulink.com/img/icon/accept.png">');
       } else {
         $('#signup-email').parent().parent().find('td:last-child').empty().append('<img alt="!" title="This email address is unavailable or invalid." src="http://s.vocabulink.com/img/icon/exclamation.png">');
       }
     });
  });
  popup.submit(function (e) {
    popup.mask('Signing up...');
    e.preventDefault();
    $.post($(this).attr('action'), $(this).serialize())
     .done(function () {location.reload();})
     .fail(function (xhr) {popup.unmask(); V.toastError(xhr.responseText, true);});
    return false;
  });
}

V.contactPopup = function () {
  var content = $('<div><h1>Contact Us</h1>'
          + '<form id="contact-form" action="/contact" method="post">'
            + '<input type="hidden" name="url" value="' + window.location + '">'
            + '<table>'
              + '<tr><th><label>Email:</label></th><td><input type="email" name="email" required autofocus style="width: 295px"></td></tr>'
              + '<tr><th><label>Message:</label></th><td><textarea name="message" required style="width: 300px"></textarea></td></tr>'
              + '<tr><td colspan="2" style="text-align: right"><input class="light" type="submit" value="Send" style="margin-bottom: 1em"></td></tr>'
            + '</table>'
          + '</form>'
        + '</div>');
  if (V.memberGravatar()) {
    content.find('tr:first-child').remove();
  }
  $.modal(content);
  var form = $('#contact-form');
  var modal = $('#simplemodal-container');
  form.minform()
      .submit(function (e) {
        modal.mask('Sending...');
        e.preventDefault();
        $.post($(this).attr('action'), $(this).serialize())
          .done(function () {$.modal.close(); V.toastSuccess('Message sent.');})
          .fail(function (xhr) {modal.unmask(); V.toastError(xhr.responseText, true);});
        return false;
      });
};

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

  $('.contact-us').live('click', function () {V.contactPopup(); return false;});
});

})(jQuery);

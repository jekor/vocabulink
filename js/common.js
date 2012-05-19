// Copyright 2009, 2010, 2011, 2012 Chris Forno
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

V.loggedIn = function () {
  return V.memberName !== null;
};

V.memberGravatar = function (size) {
  if (V.gravatarHash) {
    return 'http://www.gravatar.com/avatar/' + V.gravatarHash + '?s=' + size + '&d=wavatar&r=x';
  } else {
    return null;
  }
};

V.verified = function () {return V.memberGravatar(0);};

// http://stackoverflow.com/questions/647259/javascript-query-string
function queryString() {
  var result = {},
      queryString = location.search.substring(1),
      re = /([^&=]+)(=([^&]*))?/g,
      m;
  while (m = re.exec(queryString)) {
    result[decodeURIComponent(m[1])] = m[3] ? decodeURIComponent(m[3]) : true;
  }
  return result;
};
V.query = queryString();

V.incrLinksToReview = function (by) {
  var num = parseInt($('#head .review-box strong').text(), 10) + parseInt(by, 10);
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
  var content = $(
    '<div><h1>Lost Password</h1>'
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
    '<form id="login-popup" action="https://www.vocabulink.com/member/login" method="post">'
    + '<table>'
      + '<tr><td><label for="login-username">Username:</label></td><td><input id="login-username" name="username" required autofocus></td></tr>'
      + '<tr><td><label for="login-password">Password:</label></td><td><input id="login-password" type="password" name="password" required></td></tr>'
    + '</table>'
    + '<input type="submit" value="Login" class="dark">'
    + '<a class="cancel" href="">lost password?</a>'
  + '</form>').appendTo(headBar);
  popup.css('top', headBar.offset().top + headBar.outerHeight())
       .css('left', headBar.offset().left + headBar.outerWidth() - $('#login-popup').outerWidth() - 3)
       .find('.cancel').click(function () {
         $(this).parent().remove();
         lostPasswordPopup();
         return false;
       });
  popup.minform();
};

V.signupPopup = function() {
  var headBar = $('#head-bar');
  var usernameOK = false;
  var emailOK = false;
  var popup = $(
    '<form id="signup-popup" action="https://www.vocabulink.com/member/signup" method="post">'
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
  popup.submit(function () {
    if (!usernameOK) {
      V.toastError("Your chosen username is unavailable or invalid.");
      return false;
    }
    if (!emailOK) {
      V.toastError("Your chosen email address is unavailable or invalid.");
      return false;
    }
  });
  $('#signup-username').change(function () {
    var username = $(this).val();
    $.get('http://www.vocabulink.com/user/' + username + '/available')
     .done(function (available) {
       if (available) {
         $('#signup-username').parent().parent().find('td:last-child').empty().append('<img alt="✓" title="This username is available." src="http://s.vocabulink.com/img/icon/accept.png">');
         usernameOK = true;
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
         emailOK = true;
       } else {
         $('#signup-email').parent().parent().find('td:last-child').empty().append('<img alt="!" title="This email address is unavailable or invalid." src="http://s.vocabulink.com/img/icon/exclamation.png">');
       }
     });
  });
}

V.contactPopup = function () {
  var content = $(
    '<div><h1>Contact Us</h1>'
    + '<form id="contact-form" action="/contact" method="post">'
      + '<input type="hidden" name="url">'
      + '<table>'
        + '<tr><th><label>Email:</label></th><td><input type="email" name="email" required autofocus style="width: 295px"></td></tr>'
        + '<tr><th><label>Message:</label></th><td><textarea name="message" required style="width: 300px"></textarea></td></tr>'
        + '<tr><td colspan="2" style="text-align: right"><input class="light" type="submit" value="Send" style="margin-bottom: 1em"></td></tr>'
      + '</table>'
    + '</form>'
  + '</div>');
  content.find('input[name=url]').val(window.location);
  if (V.verified()) {
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

V.verificationPopup = function () {
  var content = $(
    '<div><h1>Email Verification Required</h1>'
    + '<p>In order to interact with Vocabulink, you need to confirm your email address.</p>'
    + '<form method="post" action="/member/confirmation" style="margin-bottom: 2.6em;">If you haven\'t received a confirmation email: <input class="button light" type="submit" value="Resend Confirmation Email"></form>'
  + '</div>'
  );
  $.modal(content);
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
    return false;
  });

  // Check for signals in the query string.
  if (V.query['badlogin'] && !V.loggedIn()) {
    V.toastError("Username and password do not match (or don't exist).");
    V.loginPopup();
  }
  if (V.query['signedup']) {
    V.toastSuccess("Welcome! Please check your email to confirm your account.", true);
  }
  if (V.query['emailconfirmed']) {
    V.toastSuccess("Congratulations! You've confirmed your account.", true);
  }

  $('#signup-button').click(function () {
    if ($('#signup-popup').length) {
      $('#signup-popup').remove();
    } else {
      $('#login-popup').remove();
      V.signupPopup();
    }
    return false;
  });

  // Hook up any buttons that require login to popup the login box.
  if (!V.loggedIn()) {
    $('.login-required').live('click', function () {
      if (!$('#login-popup').length) {
        V.loginPopup();
      }
      return false;
    });
  } else {
    // Similarly, if there are any buttons that require verification, hook those up.
    if (!V.verified()) {
      $('.verified').live('click', function () {
        V.verificationPopup();
        return false;
      });
    }
  }

  $('.contact-us').live('click', function () {V.contactPopup(); return false;});
});

})(jQuery);

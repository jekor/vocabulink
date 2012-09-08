          // when (isNothing m) $ div ! id "signup-invitation" $ do
          //   p $ "Once you're created an account, you'll get:"
          //   unordList [ "Access to More Spanish Words"
          //             , "Automatically Scheduled Reviews"
          //             , "A Dashboard and Study Stats"
          //             ]

(function ($) {

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
            .done(function () {$.modal.close(); V.toastMessage('success', 'Password recovery instructions sent.');})
            .fail(function (xhr) {modal.unmask(); V.toastMessage('error', xhr.responseText, true);});
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
        V.toastMessage('error', 'Your chosen username is unavailable or invalid.');
        return false;
      }
      if (!emailOK) {
        V.toastMessage('error', 'Your chosen email address is unavailable or invalid.');
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
    // Populate the form with any already studied links.
    $('<input type="hidden" name="learned">').val(localStorage['learnQueue']).appendTo(popup);
  };

  $(function () {

    $('#login-button').click(function () {
      if ($('#login-popup').length) {
        $('#login-popup').remove();
      } else {
        $('#signup-popup').remove();
        V.loginPopup();
      }
      return false;
    });

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
    $('.login-required').live('click', function () {
      if (!$('#login-popup').length) {
        V.loginPopup();
      }
      return false;
    });

    // Check for signals in the query string.
    if (V.query.badlogin) {
      V.toastMessage('error', "Username and password do not match (or don't exist).");
      V.loginPopup();
    }

  });

})(jQuery);

(function ($) {

  function loginPopup() {
    var form = $(
      '<form id="login" action="https://www.vocabulink.com/member/login" method="post">'
      + '<h1>Login</h1>'
      + '<table>'
        + '<tr><td><label for="login-userid">Username or Email:</label></td><td><input id="login-userid" name="userid" type="text" required autofocus></td></tr>'
        + '<tr><td><label for="login-password">Password:</label></td><td><input id="login-password" type="password" name="password" required></td></tr>'
      + '</table>'
      + '<a id="lost-password" href="" style="font-size: 10pt; display: inline-block; margin-top: 1.5em;">lost password?</a>'
      + '<input type="submit" value="Login" class="faint-gradient-button green">'
    + '</form>').minform();
    $('#lost-password', form).click(function () {
      form.attr('action', '/member/password/reset');
      $('table', form).empty().prepend('<tr><td><label for="login-email">Email:</label></td><td><input id="login-email" name="email" type="email" required autofocus></td></tr>').before('<p style="margin-bottom: 0">Enter the email address you signed up with.</p>');
      $('#lost-password', form).remove();
      $('input[type=submit]', form).attr('value', 'Recover Password');
      form.submit(function (e) {
        form.mask('Sending...');
        e.preventDefault();
        $.post($(this).attr('action'), $(this).serialize())
          .done(function () {form.trigger('reveal:close'); toast('success', 'Password recovery instructions emailed.');})
          .fail(function (xhr) {form.unmask(); toast('error', 'Failed to send password recovery email.', true);});
        return false;
      });
      return false;
    });
    return V.modal(form);
  }

  V.signupForm = function () {
    var usernameOK = false;
    var emailOK = false;
    var form = $(
      '<form id="signup" action="https://www.vocabulink.com/member/signup" method="post">'
      + '<table>'
        + '<tr><td><label for="signup-username">Username:</label></td><td><input id="signup-username" type="text" name="username" required autofocus minlength="3" maxlength="32" tabindex="1"></td><td></td></tr>'
        + '<tr><td><label for="signup-email">Email:</label></td><td><input id="signup-email" type="email" name="email" required tabindex="2"></td><td></td></tr>'
        + '<tr><td><label for="signup-password">Password:</label></td><td><input id="signup-password" type="password" name="password" required tabindex="3"></td><td></td></tr>'
        + '<tr><td colspan="2"><label for="signup-terms">I agree to the <a href="/terms-of-use" target="_blank">Terms of Use</a>.</label><input id="signup-terms" name="terms" type="checkbox" required tabindex="4"></td></tr>'
        + '<tr><td colspan="3" style="text-align: center"><input type="submit" value="Sign Up for Free" class="faint-gradient-button green" tabindex="5"></td></tr>'
      + '</table>'
    + '</form>').minform();
    form.submit(function () {
      if (!usernameOK) {
        toast('error', 'Your chosen username is unavailable or invalid.');
        return false;
      }
      if (!emailOK) {
        toast('error', 'Your chosen email address is unavailable or invalid.');
        return false;
      }
    });
    $('#signup-username', form).change(function () {
      var username = $(this).val();
      $.get('http://www.vocabulink.com/user/' + username + '/available')
       .done(function (available) {
         var statusTd = $('#signup-username', form).parent().parent().find('td:last-child');
         if (available) {
           statusTd.empty().append('<i class="sprite sprite-icon-accept" alt="✓" title="This username is available."></i>');
           usernameOK = true;
         } else {
           statusTd.empty().append('<i class="sprite sprite-icon-exclamation" alt="!" title="This username is unavailable or invalid."></i>');
           usernameOK = false;
         }
       });
    });
    $('#signup-email', form).change(function () {
      var email = $(this).val();
      $.get('http://www.vocabulink.com/email/' + email + '/available')
       .done(function (available) {
         var statusTd = $('#signup-email', form).parent().parent().find('td:last-child');
         if (available) {
           statusTd.empty().append('<i class="sprite sprite-icon-accept" alt="✓" title="This email address is valid."></i>');
           emailOK = true;
         } else {
           statusTd.empty().append('<i class="sprite sprite-icon-exclamation" alt="!" title="This email address is unavailable or invalid."></i>');
           emailOK = false;
         }
       });
    });
    // Populate the form with any already studied links.
    $('<input type="hidden" name="learned">').val(localStorage['learnQueue']).appendTo(form);
    return form;
  };

  function signupPopup() {
    return V.modal(V.signupForm().prepend('<h1>Join Vocabulink</h1>'));
  }

  $(function () {

    $('#login-button').click(function () {
      loginPopup();
      return false;
    });

    $('#signup-button').click(function () {
      signupPopup();
      return false;
    });

    // Hook up any buttons that require login to popup the login box.
    $('.login-required').live('click', function () {loginPopup(); return false;});
  });

})(jQuery);

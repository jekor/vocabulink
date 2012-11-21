(function ($) {

  $(function () {
    if (V.loggedIn()) {
      $('#change-email').click(function (e) {
        e.preventDefault();
        var emailOK = false;
        var form = $(
          '<form id="change-email-form" action="https://www.vocabulink.com/member/email/change" method="post">'
          + '<table>'
            + '<tr><td><label for="email">New Email Address:</label></td><td><input id="email" name="email" type="email" required autofocus></td><td></td></tr>'
            + '<tr><td><label for="password">Password:</label></td><td><input id="password" type="password" name="password" required></td><td></td></tr>'
            + '<tr><td></td><td colspan="2"><input type="submit" value="Change Email Address" class="faint-gradient-button green"></td></tr>'
          + '</table>'
        + '</form>').minform();
        form.modal();
        form.submit(function () {
          if (!emailOK) {
            V.toastMessage('error', 'Your chosen email address is unavailable or invalid.');
            return false;
          }
        });
        $('#email', form).change(function () {
          var email = $(this).val();
          $.get('http://www.vocabulink.com/email/' + email + '/available')
           .done(function (available) {
            var statusTd = $('#email', form).parent().parent().find('td:last-child');
            if (available) {
              statusTd.empty().append('<i class="sprite sprite-icon-accept" alt="✓" title="This email address is valid and available."></i>');
              emailOK = true;
            } else {
              statusTd.empty().append('<i class="sprite sprite-icon-exclamation" alt="!" title="This email address is unavailable or invalid."></i>');
              emailOK = false;
            }
          });
        });
      });
      $('#change-password').click(function (e) {
        e.preventDefault();
        var form = $(
          '<form id="change-password-form" action="https://www.vocabulink.com/member/password/change" method="post">'
          + '<table>'
            + '<tr><td><label for="old-password">Old Password:</label></td><td><input id="old-password" name="old-password" type="password" required autofocus></td></tr>'
            + '<tr><td><label for="new-password">New Password:</label></td><td><input id="new-password" name="new-password" type="password" required></td></tr>'
            + '<tr><td></td><td><input type="submit" value="Change Password" class="faint-gradient-button green"></td></tr>'
          + '</table>'
        + '</form>').minform();
        form.modal();
      });
      $('#delete-account').click(function (e) {
        e.preventDefault();
        var form = $(
          '<form id="delete-account-form" action="https://www.vocabulink.com/member/delete" method="post">'
          + '<p>Deleting your account will delete the following:</p>'
          + '<ul>'
            + '<li>your username and user page</li>'
            + '<li>all of your study data</li>'
            + '<li>any stories created by you</li>'
            + '<li>any comments written by you</li>'
            + '<li>replies to any of your comments</li>'
          + '</ul>'
          + '<p>Deletions are immediate and irreversible!</p>'
          + '<table>'
            + '<tr><td><label for="password">Password:</label></td><td><input id="password" name="password" type="password" required autofocus></td></tr>'
            + '<tr><td></td><td><input type="submit" value="Delete Account" class="faint-gradient-button green"></td></tr>'
          + '</table>'
        + '</form>').minform();
        form.modal();
      });
    }
  });

})(jQuery);

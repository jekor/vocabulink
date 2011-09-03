(function ($) {

  var form = $(
    '<form action="/member/password" method="post">'
    + '<label>Choose a new password:</label>'
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


})(jQuery);
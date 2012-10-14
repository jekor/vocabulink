(function ($) {

  V.incrLinksToReview = function (by) {
    var num = parseInt($('#head .review-box strong').text(), 10) + parseInt(by, 10);
    if (num === 1) {
      $('#head .review-box').empty().append('<strong>1</strong> link to review');
    } else {
      $('#head .review-box').empty().append('<strong>' + num + '</strong> links to review');
    }
  };

  $(function () {
    // Check for signals in the query string.
    if (V.query.emailchanged) {
      V.toastMessage('success', "Email address changed successfully. Please check your email to confirm the change.", true);
    }
    if (V.query.badpassword) {
      V.toastMessage('error', "Wrong password.", true);
    }
    if (V.query.emailchangefailed) {
      V.toastMessage('error', "We're sorry. We encountered an unknown error trying to change your email address.", true);
    }
    if (V.query.passwordchanged) {
      V.toastMessage('success', "Password changed successfully.", true);
    }
  });

})(jQuery);

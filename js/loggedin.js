(function ($) {

  V.incrLinksToReview = function (by) {
    var num = parseInt($('#head .review-box strong').text(), 10) + parseInt(by, 10);
    if (num === 1) {
      $('#head .review-box').empty().append('<strong>1</strong> link to review');
    } else {
      $('#head .review-box').empty().append('<strong>' + num + '</strong> links to review');
    }
  };

})(jQuery);

(function ($) {

  V.syncLinks = function () {
    var retain = V.getLocal('retain', {});
    if (retain != {}) {
      // Send retained link numbers and reviews, get back extra retain and pendingReviews.
      var request = {'retain': $.map(Object.keys(retain), function (k) {return parseInt(k, 10);})
                    };
      $.ajax({'url': '/review/sync'
             ,'type': 'POST'
             ,'data': JSON.stringify(request)
             ,'contentType': 'application/json; charset=UTF-8'
             ,'dataType': 'json'
             ,'success': function (resp) {
                V.setLocal('retain', $.extend(retain, resp['unretained']));
              }
             });
    }
    V.setLocal('lastSync', Date.now());
  }

  V.incrLinksToReview = function (by) {
    var num = parseInt($('#head .review-box strong').text(), 10) + parseInt(by, 10);
    if (num === 1) {
      $('#head .review-box').empty().append('<strong>1</strong> link to review');
    } else {
      $('#head .review-box').empty().append('<strong>' + num + '</strong> links to review');
    }
  };

  $(function () {
    if (Date.now() > V.getLocal('lastSync', 0) + 86400000) {
      V.syncLinks();
    }
  });

})(jQuery);

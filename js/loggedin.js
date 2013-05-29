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

  $(function () {
    if (Date.now() > V.getLocal('lastSync', 0) + 86400000) {
      V.syncLinks();
    }
  });

})(jQuery);

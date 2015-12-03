(function ($) {
  V.syncLinks = function () {
    var retain = V.getLocal('retain', []);
    // Send the server the list of links that we've retained. The server will
    // reply with any links we didn't send that are retained server-side, as
    // well as a list of links that we think we've retained but that it has no
    // record of.
    $.ajax({'url': '/review/sync'
           ,'type': 'POST'
           ,'data': JSON.stringify(retain)
           ,'contentType': 'application/json; charset=UTF-8'
           ,'dataType': 'json'
           ,'success': function (resp) {
              // Add all links in resp[0].
              retain = retain.concat(resp[0]);
              // Delete all links in resp[1].
              retain = retain.filter(function (n) {return $.inArray(resp[1], n) === -1;});
              // For good measure, unique values only.
              retain = retain.filter(function(n, i, a){
                return i == a.indexOf(n);
              });
              V.setLocal('retain', retain);
            }
           });
    V.setLocal('lastSync', Date.now());
  }

  $(function () {
    if (Date.now() > V.getLocal('lastSync', 0) + 86400000) {
      V.syncLinks();
    }
  });

})(jQuery);

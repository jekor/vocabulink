(function ($) {
  var icons = {'success': '✓'
              ,'error': '!'
              ,'notice': 'i'};
  window.toast = function (type, message, sticky) {
    var toasts = $('#toasts');
    if (!toasts.length) {
      toasts = $('<div id="toasts"></div>').appendTo($('body'));
    }
    var toast = $('<div class="toast ' + type + '"></div>').appendTo(toasts);
    toast.text(message).prepend('<i class="sprite sprite-toast-' + type + '" alt="' + icons[type] + '"></i>');
    var fadeAway = function () {
      toast.fadeOut(400, function () {toast.remove();});
      $('#toasts:empty').remove();
    };
    toast.click(fadeAway);
    if (!sticky) {
      var timer = setTimeout(fadeAway, 5000);
      toast.hover(function () {
        clearTimeout(timer);
      }, function () {
        timer = setTimeout(fadeAway, 5000);
      });
    }
    toast.fadeIn(400);
  };
})(jQuery);

(function ($) {
  function markRetained() {
    var links = V.getLocal('retain', {});
    $('#book .page.left a').each(function (_, el) {
      var linkNumber = $(el).attr('href').split('/').pop();
      if (typeof links[linkNumber] !== 'undefined') {
        $(el).addClass('retained');
      }
    });
  }

  $(function () {
    markRetained();
    $('#book a[href^="/link/"]').click(function (e) {
      e.preventDefault();
      $('#book .page.right').mask('Loading...');
      var linkEl = $(this);
      $.ajax({'url': linkEl.attr('href')
             ,'dataType': 'json'
             ,'success': function (link) {
                $('#book .page.right').unmask();
                $('#book .page.right').empty().append(
                  '<h2>' + link.learn + ' '
                  + '<button class="pronounce button light">'
                    + '<audio>'
                      + '<source src="//s.vocabulink.com/audio/pronunciation/' + link.number + '.ogg"></source>'
                      + '<source src="//s.vocabulink.com/audio/pronunciation/' + link.number + '.mp3"></source>'
                    + '</audio>'
                    + '<i class="sprite sprite-icon-audio"></i>'
                  + '</button>'
                + '</h2>'
                + '<h3>' + link.known + '</h3>'
                );
                if (link.soundalike) {
                  $('<p>soundalike</p>').appendTo('#book .page.right');
                }
                if (link.word) {
                  $('<p>linkword: <em>' + link.word + '</em></p>').appendTo('#book .page.right');
                  $.ajax({'url': '/link/' + link.number + '/stories'
                         ,'success': function (html) {
                           console.log('hi');
                           console.log(html);
                           $(html).appendTo('#book .page.right');
                          }
                         });
                }
                V.retainLink(link);
                markRetained();
              }
      });
    });
  });
})(jQuery);

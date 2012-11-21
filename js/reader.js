(function ($) {
  $(function () {
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
                      + '<source src="//s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.ogg"></source>'
                      + '<source src="//s.vocabulink.com/audio/pronunciation/' + link.linkNumber + '.mp3"></source>'
                    + '</audio>'
                    + '<i class="sprite sprite-icon-audio"></i>'
                  + '</button>'
                + '</h2>'
                + '<h3>' + link.known + '</h3>'
                );
                if (link.soundalike) {
                  $('<p>soundalike</p>').appendTo('#book .page.right');
                }
                if (link.linkword) {
                  $('<p>linkword: <em>' + link.linkword + '</em></p>').appendTo('#book .page.right');
                }
                for (var i = 0; i < link.stories.length; i++) {
                  var storyHtml = $(
                    '<div class="linkword-story">'
                    + '<a id="' + link.stories[i][0] + '"></a>'
                    + '<blockquote>' + link.stories[i][1] + '</blockquote>'
                    + '<div class="signature">'
                      + '<a>'
                        + '<img class="avatar" width="32" height="32" src="http://www.gravatar.com/avatar/' + link.stories[i][2][1] + '?s=32&d=wavatar&r=x">'
                      + '</a>'
                      + '<div class="details">'
                        + '<a class="username"></a>'
                        + '<br>'
                        + '<span class="date">' + link.stories[i][3] + '</span>'
                      + '</div>'
                    + '</div>'
                  + '</div>');
                  $('.signature a', storyHtml).attr('href', '/user/' + link.stories[i][2][0]);
                  $('a.username', storyHtml).text(link.stories[i][2][0]);
                  storyHtml.appendTo('#book .page.right');
                }
              }
      });
    });
  });
})(jQuery);

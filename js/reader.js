(function ($) {
  function markRetained() {
    var retain = V.getLocal('retain', []);
    $('#book .page.left a').each(function (_, el) {
      var linkNumber = parseInt($(el).attr('href').split('/').pop(), 10);
      if ($.inArray(linkNumber, retain) !== -1) {
        $(el).addClass('retained');
      }
    });
  }

  function flyToReview(wordEl, nextAction) {
    var start = wordEl.offset();
    var end = $('.review-box').offset();
    // Fly the word from its element into the cloud.
    var flier = wordEl.clone().css({'position': 'absolute'
                                   ,'left': wordEl.position().left
                                   ,'top': wordEl.position().top}).insertAfter(wordEl);
    // Fly into position.
    flier.animate({'left': '+=' + Math.round(end.left - start.left)
                  ,'top': '+=' + Math.round(end.top - start.top)
                  ,'font-size': '6pt'
                  }, 'slow', 'swing', function () {
                    // Now, pop the review count.
                    flier.remove();
                    nextAction();
                  });
  }

  function tourReader() {
      $(this).closest('.tip').remove();
      var tip = $('<div class="tip">'
                  + '<p>This is the ' + learnLanguage + ' word you\'re going to learn. You can click the speaker icon next to it to hear its pronunciation.</p>'
                  + '<p style="text-align: right"><button class="button dark">Next: Translation</button></p>'
                + '</div>').css({'max-width': '16em'});
      $('button', tip).click(function () {
        $(this).closest('.tip').remove();
        var tip = $('<div class="tip">'
                    + '<p>This is the ' + knownLanguage + ' translation. It\'s what you\'re going to try to remember (you will be quizzed on it later).</p>'
                    + '<p style="text-align: right"><button class="button dark">Next: Linkword</button></p>'
                  + '</div>').css({'max-width': '16em'});
        $('button', tip).click(function () {
          $(this).closest('.tip').remove();
          var tip = $('<div class="tip">'
                      + '<p>And this is the "linkword". It\'s a word or phrase in ' + knownLanguage + ' that sounds a little like the ' + learnLanguage + ' word. With a story, it\'s going to link the sound of the ' + learnLanguage + ' word to its meaning.</p>'
                      + '<p style="text-align: right"><button class="button dark">Next: Stories</button></p>'
                    + '</div>').css({'max-width': '20em'});
          $('button', tip).click(function () {
            $(this).closest('.tip').remove();
            var tip = $('<div class="tip">'
                        + '<p>These are the linkword stories. Read the story and notice that it uses both the linkword and the translation. Why read the story? Because your brain remembers stories better than isolated words.</p>'
                        + '<p style="text-align: right"><button class="button dark">Next: Finish</button></p>'
                      + '</div>').css({'max-width': '20em'});
            $('button', tip).click(function () {
              $(this).closest('.tip').remove();
              var tip = $('<div class="tip">'
                          + '<p>Click this button once you think you\'ve committed this word to memory, and we\'ll move on to the next word.</p>'
                        + '</div>').css({'max-width': '20em'});
              $('#confirm').one('click', function () {
                $('.tip').remove();
              });
              tooltipBelow(tip, $('#confirm'));
            });
            tooltipAbove(tip, $('.linkword-story-container:first'));
            return false;
          });
          tooltipBelow(tip, $('h1 .link'));
          return false;
        });
        tooltipBelow(tip, $('h1.link .familiar'));
        return false;
      });
      tooltipBelow(tip, $('h1.link .foreign'));
  }

  function tooltipBelow(source, target) {
    var pos = target.position();
    var width = target.outerWidth();
    target.after(source);
    var x = pos.left + (width - source.outerWidth()) / 2;
    var y = pos.top + target.outerHeight() + 10;
    source.css({'position': 'absolute', 'top': y, 'left': x});
    $('<div class="nib top"></div>').prependTo(source).css({'position': 'absolute', 'top': -14, 'left': (source.outerWidth() - 14) / 2});
    $('<a href="" class="close">x</a>').prependTo(source).click(function () {
      source.remove();
      return false;
    });
  }

  function tooltipAbove(source, target) {
    var pos = target.position();
    var width = target.outerWidth();
    target.after(source);
    var x = pos.left + (width - source.outerWidth()) / 2;
    var y = pos.top - 10 - source.outerHeight();
    source.css({'position': 'absolute', 'top': y, 'left': x});
    $('<div class="nib bottom"></div>').prependTo(source).css({'position': 'absolute', 'bottom': -14, 'left': (source.outerWidth() - 14) / 2});
    $('<a href="" class="close">x</a>').prependTo(source).click(function () {
      source.remove();
      return false;
    });
  }

  $(function () {
    markRetained();
    $('#book a[href^="/link/"]').click(function (e) {
      e.preventDefault();
      $('#book .page.right').mask('Loading...');
      var linkEl = $(this);
      var linkNumber = parseInt(linkEl.attr('href').split('/').pop(), 10);
      $.ajax(linkEl.attr('href') + '/compact')
       .done(function (html) {
         $('#book .page.right').unmask();
         $('#book .page.right').empty().append($(html));
         if (V.retainLink(linkNumber)) {
           flyToReview($('#book .page.right .learn'), function () {V.incrReviewCount(1);});
         }
         markRetained();
      });
    });
  });
})(jQuery);

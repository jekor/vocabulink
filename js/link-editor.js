// Copyright 2008, 2009, 2010, 2011 Chris Forno
//
// This file is part of Vocabulink.
//
// Vocabulink is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

(function ($) {

var changeLinkType = function () {
  var newType = $(this).val();
  $(this).parents('h1').removeClass('linkword').removeClass('soundalike').removeClass('association')
         .addClass(newType);
  var linkword = $(this).parent().find('input');
  if (newType === 'linkword') {
    linkword.prop('required', null).css('visibility', 'visible');
  } else {
    linkword.removeAttr('required').css('visibility', 'hidden');
  }
};

var fetchPronunciation = function (lang, word, callback, notFound) {
  if (fetchPronunciation.request) {
    fetchPronunciation.request.abort();
  }
  fetchPronunciation.request = $.ajax({url: '/pronunciations/' + lang + '/' + word
                                      ,dataType: 'json'
                                      ,success: function (data) {
                                        if (data.items.length === 0) {
                                          notFound();
                                        } else {
                                          callback(data.items);
                                        }
                                      }
                                      ,error: notFound
                                      });
};

var changePronunciation = function () {
  var foreign = $('h1.link.edit .foreign');
  var word = foreign.find('input');
  var lang = foreign.find('select');
  if (!word.isEmpty() && !lang.isEmpty()) {
    $('#pronounce', foreign).remove();
    var pronunciation = $(
      '<button id="pronounce" class="button light">'
      + '<audio>'
      + '</audio>'
      + '<img src="http://s.vocabulink.lan/img/loading.gif">'
    + '</button>').insertAfter(word);
    fetchPronunciation(lang.val(), word.val(), function (ps) {
      // For now, take the first pronunciation.
      var p = ps[0];
      pronunciation.find('audio').empty()
        .append('<source src="' + p.pathogg + '"></source>'
              + '<source src="' + p.pathmp3 + '"></source>');
      pronunciation.find('img').attr('src', 'http://s.vocabulink.lan/img/icon/audio.png');
      pronunciation.append('<input type="hidden" name="ogg" value="' + p.pathogg + '">'
                         + '<input type="hidden" name="mp3" value="' + p.pathmp3 + '">');
    }, function () {
      pronunciation.remove();
    });
  }
};

$(function () {
  $('h1.link.edit .link select').change(changeLinkType).change();
  $('#body form').minform();
  $('[name="foreign"], [name="foreign-lang"]').change(changePronunciation);
  changePronunciation();
  $('#pronounce').live('click', function () {$('audio', this)[0].play(); return false;});
});

})(jQuery);

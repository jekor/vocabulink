// Html5 Form Plugin - jQuery plugin
// HTML5 form Validation form Internet Explorer & Firefox
// Version 0.1 / English
//
// forked from "Html5 Form Plugin" written by Matias Mancini http://www.matiasmancini.com.ar
// 
// Copyright (c) 2011 Chris Forno (http://jekor.com/)
// Copyright (c) 2010 Matias Mancini (http://www.matiasmancini.com.ar)
// Dual licensed under the MIT (MIT-LICENSE.txt) and GPL (GPL-LICENSE.txt) licenses.

// Supports:
// * required
// * placeholder

// The "placeheld" class will set on empty inputs that have a placeholder
// attribute. You need to add CSS styling for them yourself. Recommended:
// .placeheld {
//   color: #A0A0A0;
//   font-style: italic;
// }

(function ($) {

  var placehold = function (input) {
    input.val(input.attr('placeholder'));
    input.addClass('placeheld');
  };

  $.fn.html5form = function () {
    $(this).each(function () {
      var form = $(this);

      // placeholder
      $('[placeholder]', form).each (function () {
        placehold($(this));
                          
        $(this).bind('focus', function () {
          if($(this).attr('value') == $(this).attr('placeholder')){
            $(this).attr('value', '');   
            $(this).removeClass('placeheld');
          }
        });

        $(this).bind('blur', function () {
          if (this.value === ''){
            placehold($(this));
          }
        });
      });

      // autofocus
      $('[autofocus]', form).each(function () {
        $(this).focus();
        return false;
      });

      // // maxlength
      // $('[maxlength]', form).each (function () {
      //   $(this).keypress(function (e) {
      //     var c = e.which;
      //     if (c === 32 || (c >= 48 && c <= 90) || (c >= 97 && c <= 122) || (c >= 188 && c <= 222)) {
      //       if (this.value.length >= $(this).attr('maxlength')) {
      //         return false;
      //       }
      //     }
      //     return true;
      //   });
      // });

      $(this).submit(function (e) {
        // Don't submit without all required fields.
        var required = $('[required]', form);
        for (var i = 0; i < required.length; i++) {
          r = $(required[i]);
          if (r.val() === '' || r.val() == r.attr('placeholder')) {
            r.focus();
            return false;
          }
        } 

        // Clear all placeholders for non-required fields.
        $('[placeholder]:not([required])').each(function () {
          if ($(this).val() === $(this).attr('placeholder')) {
            $(this).val('');
          }
        });
      });
    });
    return $(this);
  };

})(jQuery);
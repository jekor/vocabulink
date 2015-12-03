// Copyright 2013 Chris Forno
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
  $(function () {
    $('#checkout').click(function () {
      var readerNo = $(this).attr('reader');
      var desc = $(this).attr('description');
      var price = parseInt($(this).attr('price'), 10);

      var tokenHandler = function (token) {
        if (V.loggedIn()) {
          var form = $('<form action="https://' + window.location.hostname + window.location.pathname + '" method="post"></form>');
          $('<input type="hidden" name="reader">').val(readerNo).appendTo(form);
          $('<input type="hidden" name="stripeToken">').val(token.id).appendTo(form);
          form.appendTo('body').submit();
        } else {
          var form = V.signupForm('Sign Up');
          form.prepend('<p>In order to track your progress through the book, please create a login name.</p>');
          $('<input type="hidden" name="reader">').val(readerNo).appendTo(form);
          $('<input type="hidden" name="stripeToken">').val(token.id).appendTo(form);
          var modal = V.modal(form.prepend('<h1>Almost There!</h1>'));
        }
      };

      StripeCheckout.open({key:         V.stripeAPIKey
                          ,address:     false
                          ,amount:      price
                          ,currency:    'usd'
                          ,name:        'Vocabulink'
                          ,description: desc
                          ,panelLabel:  'Pay'
                          ,token:       tokenHandler
                          });

      return false;
    });
  });
})(jQuery);
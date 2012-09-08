// Copyright 2009, 2010, 2011, 2012 Chris Forno
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

if (!window.console) {
  window.console = {
    log: function () {}
  };
}

(function ($) {

V.uniq = function (arr) {
  var ret = [];
  arr.sort();
  ret.push(arr[0])
  for (var i = 1; i < arr.length; i++) {
    if (arr[i - 1] !== arr[i]) {
      ret.push(arr[i]);
    }
  }
  return ret;
}

// Crockford's prototypal inheritance operator
V.object = function (o) {
  function F() {}
  F.prototype = o;
  return new F();
};

V.loggedIn = function () {
  return V.memberName !== null;
};

V.memberGravatar = function (size) {
  if (V.gravatarHash) {
    return 'http://www.gravatar.com/avatar/' + V.gravatarHash + '?s=' + size + '&d=wavatar&r=x';
  } else {
    return null;
  }
};

V.verified = function () {return V.memberGravatar(0);};

// http://stackoverflow.com/questions/647259/javascript-query-string
function queryString() {
  var result = {},
      query = location.search.substring(1),
      re = /([^&=]+)(=([^&]*))?/g,
      m;
  while (m = re.exec(query)) {
    result[decodeURIComponent(m[1])] = m[3] ? decodeURIComponent(m[3]) : true;
  }
  return result;
}
V.query = queryString();

V.toastMessage = function (type, msg, sticky) {
  return $().toastmessage('showToast', {'text': msg
                                       ,'type': type
                                       ,'sticky': sticky
                                       ,'position': 'top-center'
                                       });
};

V.contactPopup = function () {
  var content = $(
    '<div><h1>Contact Us</h1>'
    + '<form id="contact-form" action="/contact" method="post">'
      + '<input type="hidden" name="url">'
      + '<table>'
        + '<tr><th><label>Email:</label></th><td><input type="email" name="email" required autofocus style="width: 295px"></td></tr>'
        + '<tr><th><label>Message:</label></th><td><textarea name="message" required style="width: 300px"></textarea></td></tr>'
        + '<tr><td colspan="2" style="text-align: right"><input class="light" type="submit" value="Send" style="margin-bottom: 1em"></td></tr>'
      + '</table>'
    + '</form>'
  + '</div>');
  content.find('input[name=url]').val(window.location);
  if (V.verified()) {
    content.find('tr:first-child').remove();
  }
  $.modal(content);
  var form = $('#contact-form');
  var modal = $('#simplemodal-container');
  form.minform()
      .submit(function (e) {
        modal.mask('Sending...');
        e.preventDefault();
        $.post($(this).attr('action'), $(this).serialize())
          .done(function () {$.modal.close(); V.toastMessage('success', 'Message sent.');})
          .fail(function (xhr) {modal.unmask(); V.toastMessage('error', xhr.responseText, true);});
        return false;
      });
};

V.verificationPopup = function () {
  var content = $(
    '<div><h1>Email Verification Required</h1>'
    + '<p>In order to interact with Vocabulink, you need to confirm your email address.</p>'
    + '<form method="post" action="/member/confirmation" style="margin-bottom: 2.6em;">If you haven\'t received a confirmation email: <input class="button light" type="submit" value="Resend Confirmation Email"></form>'
  + '</div>'
  );
  $.modal(content);
};

V.getLocal = function (key, def) {
  var val = localStorage.getItem(key);
  return val ? JSON.parse(val) : def;
}

V.setLocal = function (key, val) {
  localStorage[key] = JSON.stringify(val);
}

// initialization for every page
$(function () {
  try {
    var pageTracker = _gat._getTracker("UA-73938-2");
    pageTracker._trackPageview();
  } catch(err) {}
  // Check for signals in the query string.
  if (V.query.signedup) {
    localStorage.removeItem('learnQueue');
    V.toastMessage('success', "Welcome! Please check your email to confirm your account.", true);
  }
  if (V.query.emailconfirmed) {
    V.toastMessage('success', "Congratulations! You've confirmed your account.", true);
  }

  // Hook up any buttons that require verification.
  if (!V.verified()) {
    $('.verified').live('click', function () {
      V.verificationPopup();
      return false;
    });
  }

  $('.contact-us').live('click', function () {V.contactPopup(); return false;});
});

})(jQuery);

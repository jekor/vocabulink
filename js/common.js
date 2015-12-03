// Copyright 2009, 2010, 2011, 2012, 2013 Chris Forno
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
  return V.member !== null;
};

V.memberGravatar = function (size) {
  if (V.member && V.member.hash) {
    return 'http://www.gravatar.com/avatar/' + V.member.hash + '?s=' + size + '&d=wavatar&r=x';
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

V.modal = function (el, size) {
  var $el = $(el);
  if (!size) size = 'medium';
  $el.addClass('reveal-modal').addClass(size).append('<a class="close-reveal-modal">×</a>').css('visibility', 'hidden').appendTo('body').reveal().find('.close-reveal-modal').click(function () {$el.remove();});
}

V.verificationPopup = function () {
  var content = $(
    '<div><h1>Email Verification Required</h1>'
    + '<p>In order to interact with Vocabulink, you need to confirm your email address.</p>'
    + '<form method="post" action="/member/confirmation" style="margin-bottom: 2.6em;">If you haven\'t received a confirmation email: <input class="button light" type="submit" value="Resend Confirmation Email"></form>'
  + '</div>'
  );
  V.modal(content);
};

V.getLocal = function (key, def) {
  var str = localStorage.getItem(key);
  var val = str ? JSON.parse(str) : def;
  // If retain is in the old format (an object), update it (a simple array of link numbers).
  if (key === 'retain' && $.type(val) === 'object') {
    val = $.map(Object.keys(val), function (k) {return parseInt(k, 10);});
    val = val.filter(function (v) {return !isNaN(v);});
  }
  return val;
}

V.setLocal = function (key, val) {
  localStorage[key] = JSON.stringify(val);
}

// Returns true if the value was inserted or false otherwise (if it already existed in the list).
V.insertLocal = function (key, v) {
  var val = V.getLocal(key, []);
  if ($.inArray(v, val) === -1) {
    val.push(v);
    V.setLocal(key, val);
    return true;
  } else {
    return false;
  }
}

V.displayMessage = function () {
  // Check for messages from the server.
  if ($.cookie('msg')) {
    var msg = JSON.parse($.cookie('msg'));
    if (typeof msg === 'object' && typeof msg.type === 'string' && typeof msg.msg === 'string') {
      toast(msg.type, msg.msg, true);
    }
    $.removeCookie('msg', {'path': '/', 'domain': 'www.vocabulink.com'});
  }
};

V.setMessage = function (type, msg) {
  $.cookie('msg', JSON.stringify({'type': type, 'msg': msg}), {'path': '/', 'domain': 'www.vocabulink.com'});
};

V.bounce = function (type, msg) {
  V.setMessage(type, msg);
  window.location = document.referrer ? document.referrer : 'http://www.vocabulink.com';
};

// initialization for every page
$(function () {
  try {
    var pageTracker = _gat._getTracker("UA-73938-2");
    pageTracker._trackPageview();
  } catch(err) {}

  V.displayMessage();
  // We might receive a message cookie from any AJAX request.
  $(document).ajaxComplete(V.displayMessage);

  // Hook up any buttons that require verification.
  if (!V.verified()) {
    $('.verified').live('click', function () {
      V.verificationPopup();
      return false;
    });
  }

  $('#pronounce, button.pronounce').live('click', function () {
    $(this).find('audio')[0].play();
    return false;
  });

  $('.review-box').click(function () {
    if (V.loggedIn()) {
      if (V.getReviewCount == 0) {
        toast('notice', "You don't have any words waiting for review.", false);
        return false;
      }
    } else {
      toast('notice', "Please log in or sign up to review the words you've learned.", false);
      return false;
    }
  });
});

})(jQuery);

function loginPopup() {
  var headBar = $('#head-bar');
  $('<div id="login-popup">' +
      '<a class="close-button"><span>×</span></a>' +
      '<form action="/member/login" method="post">' +
        '<table>' +
          '<tbody>' +
            '<tr><th><label>Username:</label></th><td><input type="text" name="fval0"/></td></tr>' +
            '<tr><th><label>Password:</label></th><td><input type="password" name="fval1"/></td></tr>' +
          '</tbody>' +
        '</table>' +
        '<input type="submit" value="Login"/>' +
      '</form>' +
    '</div>').insertAfter(headBar)
    .css('top', headBar.offset().top + headBar.outerHeight())
    .css('left', headBar.offset().left + headBar.outerWidth() - $('#login-popup').outerWidth() - 3)
    .find('.close-button').click(function () {
      $(this).parent().remove();
    });
}

// initialization for every page
$(document).ready(function () {
  Functional.install();
  try {
    var pageTracker = _gat._getTracker("UA-73938-2");
    pageTracker._trackPageview();
  } catch(err) {}
  $('#login-button').click(function () {
    if ($('#login-popup').length) {
      $('#login-popup').remove();
    } else {
      loginPopup();
    }
  });
});

// Crockford's prototypal inheritance operator
function object(o) {
  function F() {}
  F.prototype = o;
  return new F();
}

// This returns a component of the user's authtoken cookie or null if the
// cookie does not exist or the component is not found.
function authTokenPart(key) {
  var authToken = $.cookie('auth');
  if (authToken) {
    var match = authToken.match(new RegExp(key + '=([^&]+)'));
    if (match) {
      return match[1];
    } else {
      return null;
    }
  } else {
    return null;
  }
}

// Read the member's name from the authtoken cookie. If the member's name
// cannot be found (likely because the client is not authenticated), this
// returns null.
// We don't worry about cookie tampering. Authentication verification is
// handled by the server.
function memberName() {
  return authTokenPart('name');
}

function memberGravatar() {
  var gravHash = authTokenPart('grav');
  if (gravHash) {
    return 'http://www.gravatar.com/avatar.php?gravatar_id=' + gravHash + '&size=60&default=wavatar';
  } else {
    return null;
  }
}

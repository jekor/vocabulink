// Allow for in-page editing and administrative tasks.

connect(window, 'onload', connectButtons);

function connectButtons() {
  var revealers = getElementsByTagAndClassName('button', 'reveal');
  map(connectButton, revealers);
}

function connectButton(button) {
  var classes = getNodeAttribute(button, 'class').split(' ');
  connect(button, 'onclick', function() {
      showElement($(classes[1]));
      hideElement(button)});
}

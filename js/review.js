function drawReview(link) {
  var startTime = new Date();
  var g = drawLinkReview(link);
  var revealed = false;
  var reveal = function() {
    revealed = true;
    g.graph.remove();
    drawLink(link);
    getReviewStats(startTime);
  }
  g.node.click(reveal);
  connect(document, 'onkeyup', function(e) {
      var k = e.key();
      if (k.string == 'KEY_SPACEBAR') {
        stop();
        reveal(startTime);
      }
      var zero = 48; // key code for the '0' key
      var pad_zero = 96; // We also want to support the number pad.
      if (revealed && ((k.code >= zero && k.code <= zero + 5) ||
                       (k.code >= pad_zero && k.code <= pad_zero + 5))) {
        stop();
        var buttons = $$('#recall-buttons button');
        var button_num = k.code - (k.code < pad_zero ? zero : pad_zero);
        buttons[button_num].click();
      }
  });
}

function getReviewStats(startTime) {
  var stopTime = new Date();
  var recallTime = stopTime.getTime() - startTime.getTime();
  setNodeAttribute($('recall-time'), 'value', recallTime);
  showElement($('recall-buttons'));
  stop();
}

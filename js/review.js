addLoadEvent(setupSignals);

function setupSignals () {
  var lexemeCover = $('lexeme-cover');
  var hiddenLexeme = getNodeAttribute($('hidden-lexeme'), 'value');
  var startTime = new Date();
  connect(lexemeCover, 'onclick', partial(showLexeme, lexemeCover, hiddenLexeme, startTime));
  connect(document, 'onkeyup', function(e) {
      var k = e.key();
      switch (k.string) {
      case 'KEY_SPACEBAR':
		showLexeme(lexemeCover, hiddenLexeme, startTime);
		stop();
		break;
      default:
      }
  });
}

function showLexeme (lexemeCover, hiddenLexeme, startTime) {
  var stopTime = new Date();
  var recallTime = stopTime.getTime() - startTime.getTime();
  swapDOM(lexemeCover, document.createTextNode(hiddenLexeme));
  setNodeAttribute($('recall-time'), 'value', recallTime);
  showElement($('recall-buttons'));
  stop();
}

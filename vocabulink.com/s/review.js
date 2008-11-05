addLoadEvent(setupSignals);

function setupSignals () {
    var lexemeCover = $('lexeme-cover');
    var hiddenLexeme = getNodeAttribute($('hidden-lexeme'), 'value');
    var startTime = new Date();
    connect(lexemeCover, 'onclick', partial(showLexeme, lexemeCover, hiddenLexeme, startTime));
}

function showLexeme (lexemeCover, hiddenLexeme, startTime) {
    var stopTime = new Date();
    var recallTime = stopTime.getTime() - startTime.getTime();
    swapDOM(lexemeCover, document.createTextNode(hiddenLexeme));
    setNodeAttribute($('recall-time'), 'value', recallTime);
    stop();
}
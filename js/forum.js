connect(window, 'onload', setup);

function setup() {
  roundDivs();
}

function roundDivs() {
  roundClass('div', 'forum-group');
  var toplevelComments = $$('.comment.toplevel');
  map(roundElement, toplevelComments);
  roundElement('topics');
}

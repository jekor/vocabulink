connect(window, 'onload', setup);

function setup() {
  roundDivs();
}

function roundDivs() {
  roundClass('div', 'forum-group');
  roundClass('div', 'comment');
  roundElement('topics');
}

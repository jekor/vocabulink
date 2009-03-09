connect(window, 'onload', roundDivs);

function roundDivs() {
  roundClass('div', 'forum-group');
  roundClass('div', 'comment');
  roundElement('topics');
}

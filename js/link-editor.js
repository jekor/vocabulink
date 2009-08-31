// Copyright 2008, 2009 Chris Forno
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

$(document).ready(function() {
  var linkTypeSelector = $('select[name=fval4]:first');
  linkTypeSelector.change(showLinkEditor);
  linkTypeSelector.change();
  // $('#edit').one('click', createMnemonicEditor);
});

function showLinkEditor() {
  var linkType = this.options[this.selectedIndex].text.replace(/ /g, '-');
  $('fieldset').hide();
  $('#' + linkType).show();
}

// This is old CKEditor stuff.
// var mnemonicEditor;
//
// function createMnemonicEditor() {
// 	if (mnemonicEditor)
// 		mnemonicEditor.destroy();
//   mnemonicEditor = CKEDITOR.replace('link-details',
//     {'height': 400,
//      'removePlugins': 'pagebreak,pastefromword,print,smiley,forms,flash,about,newpage,preview,scayt,wsc,find',
//      'toolbar': [
//        ['Source','-','Save','-','Templates'],
//        ['Cut','Copy','Paste','PasteText'],
//        ['Undo','Redo','-','SelectAll','RemoveFormat'],
//        ['Maximize', 'ShowBlocks'],
//        ['Link','Unlink'],
//        '/',
//        ['Bold','Italic','Underline','Strike','-','Subscript','Superscript'],
//        ['NumberedList','BulletedList','-','Outdent','Indent','Blockquote'],
//        ['JustifyLeft','JustifyCenter','JustifyRight','JustifyBlock'],
//        ['Image','Flash','Table','HorizontalRule'],
//        '/',
//        ['Styles','Format','Font','FontSize'],
//        ['TextColor','BGColor']
//      ]});
// }


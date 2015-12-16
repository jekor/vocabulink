(function () {

function markdownTitle (markItUp, chr) {
  heading = '';
  n = $.trim(markItUp.selection||markItUp.placeHolder).length;
  for(i = 0; i < n; i++) {
    heading += chr;
  }
  return '\n'+heading;
}

V.markItUpSettings = {
  previewParserPath: '',
  onShiftEnter:	{keepDefault:false, openWith:'\n\n'},
  markupSet: [
    {name:'First Level Heading', key:'1', placeHolder:'Your title here...', icon:'text_heading_1', closeWith:function(markItUp) { return markdownTitle(markItUp, '=') } },
    {name:'Second Level Heading', key:'2', placeHolder:'Your title here...', icon:'text_heading_2', closeWith:function(markItUp) { return markdownTitle(markItUp, '-') } },
    {name:'Heading 3', key:'3', icon:'text_heading_3', openWith:'### ', placeHolder:'Your title here...' },
    {separator:'---------------' },
    {name:'Bold', key:'B', icon:'text_bold', openWith:'**', closeWith:'**'},
    {name:'Italic', key:'I', icon:'text_italic', openWith:'_', closeWith:'_'},
    {separator:'---------------' },
    {name:'Bulleted List', icon:'text_list_bullets', openWith:'- ' },
    {name:'Numeric List', icon:'text_list_numbers', openWith:function(markItUp) { return markItUp.line+'. '; } },
    {separator:'---------------' },
    {name:'Picture', key:'P', icon:'picture', replaceWith:'![[![Alternative text]!]]([![Url:!:http://]!] "[![Title]!]")'},
    {name:'Link', key:'L', icon:'link', openWith:'[', closeWith:']([![Url:!:http://]!] "[![Title]!]")', placeHolder:'Your text to link here...' },
    {separator:'---------------'},
    {name:'Quotes', icon:'user_comment', openWith:'> '},
    {name:'Code Block / Code', icon:'script_code', openWith:'(!(\t|!|`)!)', closeWith:'(!(`)!)'},
    {separator:'---------------'},
    {name:'Preview', call:'preview', icon:'tick'}
  ]
};

})();

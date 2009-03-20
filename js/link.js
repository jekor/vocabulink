connect(window, 'onload', setupSignals);

function setupSignals() {
    var linkTypeSelector = $$('select[name=input2]')[0];
    connect(linkTypeSelector, 'onchange', partial(showLinkEditor, linkTypeSelector));
    signal(linkTypeSelector, 'onchange');
}

function showLinkEditor(linkTypeSelector) {
    var linkType = linkTypeSelector.options[linkTypeSelector.selectedIndex].text.replace(/ /g, '-');
    map(hideElement, $$('fieldset'));
    showElement($(linkType));
}

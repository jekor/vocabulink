addLoadEvent(setupSignals);

function setupSignals () {
    var linkTypeSelector = $('link-type');
    connect(linkTypeSelector, 'onchange', partial(showLinkEditor, linkTypeSelector));
}

function showLinkEditor (linkTypeSelector) {
    var linkType = linkTypeSelector.value.replace(/ /g, '-');
    map(hideElement, $$('.link-editor'));
    showElement($(linkType));
}

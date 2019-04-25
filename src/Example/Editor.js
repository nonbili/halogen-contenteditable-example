exports.getSelectionRect_ = function(el) {
  var sel = window.getSelection();
  if (
    !sel.isCollapsed &&
    el.contains(sel.anchorNode) &&
    el.contains(sel.focusNode)
  ) {
    const rect = sel.getRangeAt(0).getBoundingClientRect();
    return sel.getRangeAt(0).getBoundingClientRect();
  }
};

exports.execCommand_ = function(command, param) {
  document.execCommand(command, false, param);
};

exports.queryCommandState_ = function(command) {
  return document.queryCommandState(command);
};

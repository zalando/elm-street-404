//import Result //

var _zalando$elm_street_404$Native_Offset = {
  offset: function offset (el) {
    try {
      var rect = el.getBoundingClientRect();
      return _elm_lang$core$Result$Ok({
        x: rect.left + window.pageXOffset - document.documentElement.clientLeft,
        y: rect.top + window.pageYOffset - document.documentElement.clientTop
      });
    } catch (e) {
      return _elm_lang$core$Result$Err('Could not calculate the offset');
    }
  }
};

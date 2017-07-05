"use strict";


exports.sidebarHide = function sidebarHide (sel) {
  $(sel).sidebar('hide');
};

exports.sidebarShow = function sidebarShow (sel) {
  $(sel).sidebar('show');
};

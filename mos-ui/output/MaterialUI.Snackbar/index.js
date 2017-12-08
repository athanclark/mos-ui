// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Eq = require("../Data.Eq");
var Data_Nullable = require("../Data.Nullable");
var Data_Record_Class = require("../Data.Record.Class");
var Data_Time_Duration = require("../Data.Time.Duration");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var VerticalOrigin = function (x) {
    return x;
};
var HorizontalOrigin = function (x) {
    return x;
};
var CloseReason = function (x) {
    return x;
};
var vCenter = "center";
var top = "top";
var timeout = "timeout";
var snackbar = function (dictSubrow) {
    return function (p) {
        return React.createElement($foreign.snackbarImpl)(p)([  ]);
    };
};
var right = "right";
var left = "left";
var hCenter = "center";
var eqCloseReason = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v === v1;
    };
});
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var clickaway = "clickaway";
var bottom = "bottom";
module.exports = {
    snackbar: snackbar,
    left: left,
    right: right,
    vCenter: vCenter,
    bottom: bottom,
    top: top,
    hCenter: hCenter,
    timeout: timeout,
    clickaway: clickaway,
    createClasses: createClasses,
    eqCloseReason: eqCloseReason
};

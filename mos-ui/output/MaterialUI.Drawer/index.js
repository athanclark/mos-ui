// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Record_Class = require("../Data.Record.Class");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Anchor = function (x) {
    return x;
};
var top = "top";
var right = "right";
var left = "left";
var drawer = function (dictSubrow) {
    return React.createElement($foreign.drawerImpl);
};
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var bottom = "bottom";
module.exports = {
    bottom: bottom, 
    createClasses: createClasses, 
    drawer: drawer, 
    left: left, 
    right: right, 
    top: top
};
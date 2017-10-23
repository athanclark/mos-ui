// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Record_Class = require("../Data.Record.Class");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var collapse = function (dictSubrow) {
    return React.createElement($foreign.collapseImpl);
};
module.exports = {
    collapse: collapse, 
    createClasses: createClasses
};

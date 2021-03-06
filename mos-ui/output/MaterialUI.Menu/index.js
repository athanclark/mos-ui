// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Record_Class = require("../Data.Record.Class");
var MaterialUI_Input = require("../MaterialUI.Input");
var MaterialUI_ListItem = require("../MaterialUI.ListItem");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var menuItem = function (dictSubrow) {
    return function (dictUnion) {
        return React.createElement($foreign.menuItemImpl);
    };
};
var menu = function (dictSubrow) {
    return React.createElement($foreign.menuImpl);
};
var createClassesItem = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
module.exports = {
    menu: menu,
    createClasses: createClasses,
    menuItem: menuItem,
    createClassesItem: createClassesItem
};

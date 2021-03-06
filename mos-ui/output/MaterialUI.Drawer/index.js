// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Record_Class = require("../Data.Record.Class");
var Data_Unit = require("../Data.Unit");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Type_Row = require("../Type.Row");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var DrawerType = function (x) {
    return x;
};
var Anchor = function (x) {
    return x;
};
var withStyles = function (dictSubrow) {
    return function (dictRowToList) {
        return function (dictCompileStyles) {
            return function (dictListToRow) {
                return function (stylesF) {
                    return function (createElem) {
                        return React.createElement($foreign.withStylesImpl(stylesF, React.createClassStateless(createElem)))(Data_Unit.unit)([  ]);
                    };
                };
            };
        };
    };
};
var top = "top";
var temporary = "temporary";
var right = "right";
var persistent = "persistent";
var permanent = "permanent";
var left = "left";
var drawer = function (dictSubrow) {
    return React.createElement($foreign.drawerImpl);
};
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var bottom = "bottom";
module.exports = {
    drawer: drawer,
    left: left,
    right: right,
    top: top,
    bottom: bottom,
    permanent: permanent,
    persistent: persistent,
    temporary: temporary,
    createClasses: createClasses,
    withStyles: withStyles
};

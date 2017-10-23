// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Record_Class = require("../Data.Record.Class");
var MaterialUI_Types = require("../MaterialUI.Types");
var Prelude = require("../Prelude");
var React = require("../React");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Type = function (x) {
    return x;
};
var Color = function (x) {
    return x;
};
var Alignment = function (x) {
    return x;
};
var typography = function (dictSubrow) {
    return React.createElement($foreign.typographyImpl);
};
var title = "title";
var subheading = "subheading";
var secondary = "secondary";
var right = "right";
var left = "left";
var justify = "justify";
var inheritColor = "inherit";
var inheritAlign = "inherit";
var headline = "headline";
var display4 = "display4";
var display3 = "display3";
var display2 = "display2";
var display1 = "display1";
var $$default = "default";
var createClasses = function (dictSubrow) {
    return Unsafe_Coerce.unsafeCoerce;
};
var center = "center";
var caption = "caption";
var button = "button";
var body2 = "body2";
var body1 = "body1";
var accent = "accent";
module.exports = {
    accent: accent, 
    body1: body1, 
    body2: body2, 
    button: button, 
    caption: caption, 
    center: center, 
    createClasses: createClasses, 
    "default": $$default, 
    display1: display1, 
    display2: display2, 
    display3: display3, 
    display4: display4, 
    headline: headline, 
    inheritAlign: inheritAlign, 
    inheritColor: inheritColor, 
    justify: justify, 
    left: left, 
    right: right, 
    secondary: secondary, 
    subheading: subheading, 
    title: title, 
    typography: typography
};
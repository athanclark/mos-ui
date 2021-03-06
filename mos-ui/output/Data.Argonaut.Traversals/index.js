// Generated by purs version 0.11.7
"use strict";
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Fold = require("../Data.Lens.Fold");
var Prelude = require("../Prelude");
var _JsonString = function (dictWander) {
    return function ($6) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isString)($6));
    };
};
var _JsonObject = function (dictWander) {
    return function ($7) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isObject)($7));
    };
};
var _JsonNumber = function (dictWander) {
    return function ($8) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isNumber)($8));
    };
};
var _JsonNull = function (dictWander) {
    return function ($9) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isNull)($9));
    };
};
var _JsonBoolean = function (dictWander) {
    return function ($10) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isBoolean)($10));
    };
};
var _JsonArray = function (dictWander) {
    return function ($11) {
        return Control_Category.id(Control_Category.categoryFn)(Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isArray)($11));
    };
};
module.exports = {
    _JsonNull: _JsonNull,
    _JsonBoolean: _JsonBoolean,
    _JsonNumber: _JsonNumber,
    _JsonString: _JsonString,
    _JsonArray: _JsonArray,
    _JsonObject: _JsonObject
};

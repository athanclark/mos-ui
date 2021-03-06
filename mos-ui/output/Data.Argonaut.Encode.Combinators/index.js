// Generated by purs version 0.11.7
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var extend = function (dictEncodeJson) {
    return function (v) {
        return function ($6) {
            return Data_Argonaut_Core.foldJsonObject(Data_Argonaut_Core.jsonSingletonObject(v.value0)(v.value1))(function ($7) {
                return Data_Argonaut_Core.fromObject(Data_StrMap.insert(v.value0)(v.value1)($7));
            })(Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson)($6));
        };
    };
};
var assoc = function (dictEncodeJson) {
    return function (k) {
        return function ($8) {
            return Data_Tuple.Tuple.create(k)(Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson)($8));
        };
    };
};
module.exports = {
    assoc: assoc,
    extend: extend
};

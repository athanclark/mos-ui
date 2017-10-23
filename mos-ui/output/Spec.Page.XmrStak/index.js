// Generated by purs version 0.11.6
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Data_Record_Class = require("../Data.Record.Class");
var Data_Unit = require("../Data.Unit");
var MaterialUI_Divider = require("../MaterialUI.Divider");
var MaterialUI_Typography = require("../MaterialUI.Typography");
var Prelude = require("../Prelude");
var React_DOM = require("../React.DOM");
var Thermite = require("../Thermite");
var spec = (function () {
    var render = function (dispatch) {
        return function (state) {
            return function (props) {
                return function (children) {
                    return [ MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.headline
                    })([ React_DOM.text("xmr-stak") ]), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.display1
                    })([ React_DOM.text("Status") ]), MaterialUI_Divider.divider(Data_Record_Class.srInst())({}), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.body1
                    })([ React_DOM.em([  ])([ React_DOM.text("TODO") ]) ]), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.display1
                    })([ React_DOM.text("Config") ]), MaterialUI_Divider.divider(Data_Record_Class.srInst())({}), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.body1
                    })([ React_DOM.em([  ])([ React_DOM.text("TODO") ]) ]) ];
                };
            };
        };
    };
    var performAction = function (v) {
        return function (v1) {
            return function (v2) {
                return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Data_Unit.unit);
            };
        };
    };
    return Thermite.simpleSpec(performAction)(render);
})();
var initialState = Data_Unit.unit;
module.exports = {
    initialState: initialState, 
    spec: spec
};

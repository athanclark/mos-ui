// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Record_Class = require("../Data.Record.Class");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var MaterialUI_Avatar = require("../MaterialUI.Avatar");
var MaterialUI_Chip = require("../MaterialUI.Chip");
var MaterialUI_Divider = require("../MaterialUI.Divider");
var MaterialUI_Icons_Brightness3 = require("../MaterialUI.Icons.Brightness3");
var MaterialUI_Icons_CheckCircle = require("../MaterialUI.Icons.CheckCircle");
var MaterialUI_Icons_ErrorOutline = require("../MaterialUI.Icons.ErrorOutline");
var MaterialUI_LinearProgress = require("../MaterialUI.LinearProgress");
var MaterialUI_Typography = require("../MaterialUI.Typography");
var Monerodo_MoneroD = require("../Monerodo.MoneroD");
var Prelude = require("../Prelude");
var React_DOM = require("../React.DOM");
var System_SystemD_Status = require("../System.SystemD.Status");
var Thermite = require("../Thermite");
var Types_DBus = require("../Types.DBus");
var GotSignal = (function () {
    function GotSignal(value0) {
        this.value0 = value0;
    };
    GotSignal.create = function (value0) {
        return new GotSignal(value0);
    };
    return GotSignal;
})();
var GotStatus = (function () {
    function GotStatus(value0) {
        this.value0 = value0;
    };
    GotStatus.create = function (value0) {
        return new GotStatus(value0);
    };
    return GotStatus;
})();
var spec = (function () {
    var render = function (dispatch) {
        return function (props) {
            return function (state) {
                return function (children) {
                    var status = (function () {
                        if (state.systemdStatus instanceof Data_Maybe.Nothing) {
                            return [ MaterialUI_Chip.chip(Data_Record_Class.srInst())({
                                label: React_DOM.text("not connected to mosd"),
                                avatar: MaterialUI_Avatar.avatar(Data_Record_Class.srInst())({})([ MaterialUI_Icons_ErrorOutline.errorOutlineIcon ])
                            }) ];
                        };
                        if (state.systemdStatus instanceof Data_Maybe.Just) {
                            if (state.systemdStatus.value0.activeState instanceof System_SystemD_Status.Failed) {
                                return [ MaterialUI_Chip.chip(Data_Record_Class.srInst())({
                                    avatar: MaterialUI_Avatar.avatar(Data_Record_Class.srInst())({})([ MaterialUI_Icons_ErrorOutline.errorOutlineIcon ]),
                                    label: React_DOM.text("failed")
                                }) ];
                            };
                            if (state.systemdStatus.value0.activeState instanceof System_SystemD_Status.Inactive) {
                                return [ MaterialUI_Chip.chip(Data_Record_Class.srInst())({
                                    avatar: MaterialUI_Avatar.avatar(Data_Record_Class.srInst())({})([ MaterialUI_Icons_Brightness3.brightness3Icon ]),
                                    label: React_DOM.text("inactive")
                                }) ];
                            };
                            if (state.systemdStatus.value0.activeState instanceof System_SystemD_Status.Active) {
                                if (state.syncHeight instanceof Data_Maybe.Nothing) {
                                    return [ MaterialUI_LinearProgress.linearProgress(Data_Record_Class.srInst())({
                                        mode: MaterialUI_LinearProgress.determinate,
                                        value: 0.0
                                    }), MaterialUI_Chip.chip(Data_Record_Class.srInst())({
                                        avatar: MaterialUI_Avatar.avatar(Data_Record_Class.srInst())({})([ MaterialUI_Icons_CheckCircle.checkCircleIcon ]),
                                        label: React_DOM.text("running")
                                    }) ];
                                };
                                if (state.syncHeight instanceof Data_Maybe.Just) {
                                    return [ MaterialUI_LinearProgress.linearProgress(Data_Record_Class.srInst())({
                                        mode: MaterialUI_LinearProgress.determinate,
                                        value: (Data_Int.toNumber(state.syncHeight.value0.value0) / Data_Int.toNumber(state.syncHeight.value0.value1)) * 100.0
                                    }), MaterialUI_Chip.chip(Data_Record_Class.srInst())({
                                        avatar: MaterialUI_Avatar.avatar(Data_Record_Class.srInst())({})([ MaterialUI_Icons_CheckCircle.checkCircleIcon ]),
                                        label: React_DOM.text("Sync Height: " + (Data_Show.show(Data_Show.showInt)(state.syncHeight.value0.value0) + (" / " + Data_Show.show(Data_Show.showInt)(state.syncHeight.value0.value1))))
                                    }) ];
                                };
                                throw new Error("Failed pattern match at Spec.Page.MoneroD line 84, column 23 - line 98, column 18: " + [ state.syncHeight.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Spec.Page.MoneroD line 75, column 61 - line 98, column 18: " + [ state.systemdStatus.value0.activeState.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Spec.Page.MoneroD line 70, column 18 - line 98, column 18: " + [ state.systemdStatus.constructor.name ]);
                    })();
                    var mkBody = function (x) {
                        return MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                            type: MaterialUI_Typography.body1
                        })(x);
                    };
                    return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.headline
                    })([ React_DOM.text("monerod") ]), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.display1
                    })([ React_DOM.text("Status") ]), MaterialUI_Divider.divider(Data_Record_Class.srInst())({}) ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(status)([ MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.display1
                    })([ React_DOM.text("Config") ]), MaterialUI_Divider.divider(Data_Record_Class.srInst())({}), MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.body1
                    })([ React_DOM.em([  ])([ React_DOM.text("TODO") ]) ]) ]));
                };
            };
        };
    };
    var performAction = function (action) {
        return function (props) {
            return function (state) {
                if (action instanceof GotSignal) {
                    if (action.value0.value0 instanceof Monerodo_MoneroD.SyncProgress) {
                        return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v) {
                            var $14 = {};
                            for (var $15 in v) {
                                if ({}.hasOwnProperty.call(v, $15)) {
                                    $14[$15] = v[$15];
                                };
                            };
                            $14.syncHeight = new Data_Maybe.Just(new Data_Tuple.Tuple(action.value0.value0.value0.amount, action.value0.value0.value0.total));
                            return $14;
                        }));
                    };
                    if (action.value0.value0 instanceof Monerodo_MoneroD.SyncNewTopBlock) {
                        return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v) {
                            var $20 = {};
                            for (var $21 in v) {
                                if ({}.hasOwnProperty.call(v, $21)) {
                                    $20[$21] = v[$21];
                                };
                            };
                            $20.syncHeight = new Data_Maybe.Just(new Data_Tuple.Tuple(action.value0.value0.value0.current, action.value0.value0.value0.top));
                            return $20;
                        }));
                    };
                    return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Data_Unit.unit);
                };
                if (action instanceof GotStatus) {
                    return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v) {
                        var $28 = {};
                        for (var $29 in v) {
                            if ({}.hasOwnProperty.call(v, $29)) {
                                $28[$29] = v[$29];
                            };
                        };
                        $28.systemdStatus = new Data_Maybe.Just(action.value0);
                        return $28;
                    }));
                };
                throw new Error("Failed pattern match at Spec.Page.MoneroD line 43, column 40 - line 48, column 71: " + [ action.constructor.name ]);
            };
        };
    };
    return Thermite.simpleSpec(performAction)(render);
})();
var initialState = {
    syncHeight: Data_Maybe.Nothing.value,
    systemdStatus: Data_Maybe.Nothing.value
};
module.exports = {
    initialState: initialState,
    GotSignal: GotSignal,
    GotStatus: GotStatus,
    spec: spec
};

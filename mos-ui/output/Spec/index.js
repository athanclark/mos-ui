// Generated by purs version 0.11.6
"use strict";
var Client_Constants = require("../Client.Constants");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Parallel_Class = require("../Control.Parallel.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_HTML = require("../DOM.HTML");
var DOM_HTML_Document = require("../DOM.HTML.Document");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_HTML_Window = require("../DOM.HTML.Window");
var Data_Argonaut = require("../Data.Argonaut");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Getter = require("../Data.Lens.Getter");
var Data_Lens_Internal_Forget = require("../Data.Lens.Internal.Forget");
var Data_Lens_Lens = require("../Data.Lens.Lens");
var Data_Lens_Prism = require("../Data.Lens.Prism");
var Data_Lens_Record = require("../Data.Lens.Record");
var Data_Maybe = require("../Data.Maybe");
var Data_Record_Class = require("../Data.Record.Class");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Symbol = require("../Data.Symbol");
var Data_Time_Duration = require("../Data.Time.Duration");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Electron = require("../Electron");
var Electron_Renderer = require("../Electron.Renderer");
var MaterialUI_AppBar = require("../MaterialUI.AppBar");
var MaterialUI_Button = require("../MaterialUI.Button");
var MaterialUI_InjectTapEvent = require("../MaterialUI.InjectTapEvent");
var MaterialUI_MuiThemeProvider = require("../MaterialUI.MuiThemeProvider");
var MaterialUI_Paper = require("../MaterialUI.Paper");
var MaterialUI_Toolbar = require("../MaterialUI.Toolbar");
var MaterialUI_Types = require("../MaterialUI.Types");
var MaterialUI_Typography = require("../MaterialUI.Typography");
var Prelude = require("../Prelude");
var Queue = require("../Queue");
var React = require("../React");
var React_DOM = require("../React.DOM");
var React_DOM_Props = require("../React.DOM.Props");
var ReactDOM = require("../ReactDOM");
var Signal_Channel = require("../Signal.Channel");
var Spec_Page_MoneroD = require("../Spec.Page.MoneroD");
var Spec_Page_XmrStak = require("../Spec.Page.XmrStak");
var System_SystemD_Status = require("../System.SystemD.Status");
var Thermite = require("../Thermite");
var Types_DBus = require("../Types.DBus");
var Types_Env = require("../Types.Env");
var MoneroDPage = (function () {
    function MoneroDPage() {

    };
    MoneroDPage.value = new MoneroDPage();
    return MoneroDPage;
})();
var XmrStakPage = (function () {
    function XmrStakPage() {

    };
    XmrStakPage.value = new XmrStakPage();
    return XmrStakPage;
})();
var MoneroDAction = (function () {
    function MoneroDAction(value0) {
        this.value0 = value0;
    };
    MoneroDAction.create = function (value0) {
        return new MoneroDAction(value0);
    };
    return MoneroDAction;
})();
var XmrStakAction = (function () {
    function XmrStakAction(value0) {
        this.value0 = value0;
    };
    XmrStakAction.create = function (value0) {
        return new XmrStakAction(value0);
    };
    return XmrStakAction;
})();
var MoneroD = (function () {
    function MoneroD(value0) {
        this.value0 = value0;
    };
    MoneroD.create = function (value0) {
        return new MoneroD(value0);
    };
    return MoneroD;
})();
var XmrStak = (function () {
    function XmrStak(value0) {
        this.value0 = value0;
    };
    XmrStak.create = function (value0) {
        return new XmrStak(value0);
    };
    return XmrStak;
})();
var ClickedMoneroD = (function () {
    function ClickedMoneroD() {

    };
    ClickedMoneroD.value = new ClickedMoneroD();
    return ClickedMoneroD;
})();
var ClickedXmrStak = (function () {
    function ClickedXmrStak() {

    };
    ClickedXmrStak.value = new ClickedXmrStak();
    return ClickedXmrStak;
})();
var NavAction = (function () {
    function NavAction(value0) {
        this.value0 = value0;
    };
    NavAction.create = function (value0) {
        return new NavAction(value0);
    };
    return NavAction;
})();
var IpcAction = (function () {
    function IpcAction(value0) {
        this.value0 = value0;
    };
    IpcAction.create = function (value0) {
        return new IpcAction(value0);
    };
    return IpcAction;
})();
var PageAction = (function () {
    function PageAction(value0) {
        this.value0 = value0;
    };
    PageAction.create = function (value0) {
        return new PageAction(value0);
    };
    return PageAction;
})();
var initialState = function (env) {
    return {
        currentPage: new MoneroD(Spec_Page_MoneroD.initialState), 
        pendingPage: new Data_Maybe.Just(MoneroDPage.value), 
        env: env
    };
};
var _currentPage = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.currentPage;
    })(function (v) {
        return function (v1) {
            var $42 = {};
            for (var $43 in v) {
                if ({}.hasOwnProperty.call(v, $43)) {
                    $42[$43] = v[$43];
                };
            };
            $42.currentPage = v1;
            return $42;
        };
    })(dictStrong);
};
var _XmrStakAction = function (dictChoice) {
    return Data_Lens_Prism["prism'"](XmrStakAction.create)(function (v) {
        if (v instanceof XmrStakAction) {
            return new Data_Maybe.Just(v.value0);
        };
        return Data_Maybe.Nothing.value;
    })(dictChoice);
};
var _XmrStak = function (dictChoice) {
    return Data_Lens_Prism["prism'"](XmrStak.create)(function (v) {
        if (v instanceof XmrStak) {
            return new Data_Maybe.Just(v.value0);
        };
        return Data_Maybe.Nothing.value;
    })(dictChoice);
};
var _PageAction = function (dictChoice) {
    return Data_Lens_Prism["prism'"](PageAction.create)(function (v) {
        if (v instanceof PageAction) {
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof IpcAction && v.value0 instanceof Types_DBus.SignalOutput) {
            return new Data_Maybe.Just(MoneroDAction.create(new Spec_Page_MoneroD.GotSignal(v.value0.value0)));
        };
        return Data_Maybe.Nothing.value;
    })(dictChoice);
};
var _MoneroDAction = function (dictChoice) {
    return Data_Lens_Prism["prism'"](MoneroDAction.create)(function (v) {
        if (v instanceof MoneroDAction) {
            return new Data_Maybe.Just(v.value0);
        };
        return Data_Maybe.Nothing.value;
    })(dictChoice);
};
var _MoneroD = function (dictChoice) {
    return Data_Lens_Prism["prism'"](MoneroD.create)(function (v) {
        if (v instanceof MoneroD) {
            return new Data_Maybe.Just(v.value0);
        };
        return Data_Maybe.Nothing.value;
    })(dictChoice);
};
var spec = (function () {
    var xmrStak = Thermite.focus(function (dictStrong) {
        return _currentPage(dictStrong);
    })(function (dictChoice) {
        return function ($128) {
            return _PageAction(dictChoice)(_XmrStakAction(dictChoice)($128));
        };
    })(Thermite.split(function (dictChoice) {
        return _XmrStak(dictChoice);
    })(Spec_Page_XmrStak.spec));
    var moneroD = Thermite.focus(function (dictStrong) {
        return _currentPage(dictStrong);
    })(function (dictChoice) {
        return function ($129) {
            return _PageAction(dictChoice)(_MoneroDAction(dictChoice)($129));
        };
    })(Thermite.split(function (dictChoice) {
        return _MoneroD(dictChoice);
    })(Spec_Page_MoneroD.spec));
    var performAction = function (action) {
        return function (props) {
            return function (state) {
                if (action instanceof NavAction) {
                    if (action.value0 instanceof ClickedMoneroD) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Free_Trans.monadEffFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadEffAff))(Electron_Renderer.send({
                            channel: Client_Constants.controlInput, 
                            message: Data_Argonaut_Encode_Class.encodeJson(Types_DBus.encodeJsonControlInput)(Types_DBus.GetServiceState.create(new Data_Maybe.Just(Types_DBus.ServiceMoneroD.value)))
                        })))(function () {
                            return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v) {
                                var $59 = {};
                                for (var $60 in v) {
                                    if ({}.hasOwnProperty.call(v, $60)) {
                                        $59[$60] = v[$60];
                                    };
                                };
                                $59.pendingPage = new Data_Maybe.Just(MoneroDPage.value);
                                return $59;
                            }));
                        });
                    };
                    if (action.value0 instanceof ClickedXmrStak) {
                        return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v) {
                            var $62 = {};
                            for (var $63 in v) {
                                if ({}.hasOwnProperty.call(v, $63)) {
                                    $62[$63] = v[$63];
                                };
                            };
                            $62.currentPage = new XmrStak(Spec_Page_XmrStak.initialState);
                            return $62;
                        }));
                    };
                    throw new Error("Failed pattern match at Spec line 130, column 30 - line 134, column 96: " + [ action.value0.constructor.name ]);
                };
                if (action instanceof IpcAction && action.value0 instanceof Types_DBus.ControlOutput) {
                    var v = Data_Array.head(action.value0.value0.value0);
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Free_Trans.monadEffFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadEffAff))(Control_Monad_Eff_Console.warn("Empty Service States!")))(function () {
                            return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v1) {
                                var $67 = {};
                                for (var $68 in v1) {
                                    if ({}.hasOwnProperty.call(v1, $68)) {
                                        $67[$68] = v1[$68];
                                    };
                                };
                                $67.pendingPage = Data_Maybe.Nothing.value;
                                return $67;
                            }));
                        });
                    };
                    if (v instanceof Data_Maybe.Just) {
                        if (v.value0.name === (Types_Env.getEnvData(state.env)).monerodService) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Control_Monad_Eff_Class.liftEff(Control_Monad_Free_Trans.monadEffFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadEffAff))(Control_Monad_Eff_Console.log("got monerod service! " + Data_Show.show(System_SystemD_Status.showSystemDStatus)(v.value0))))(function () {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))((function () {
                                    if (state.pendingPage instanceof Data_Maybe.Just && state.pendingPage.value0 instanceof MoneroDPage) {
                                        return Data_Functor["void"](Control_Monad_Free_Trans.functorFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.functorAff))(Control_Coroutine.cotransform(Control_Monad_Aff.monadAff)(function (v1) {
                                            var $71 = {};
                                            for (var $72 in v1) {
                                                if ({}.hasOwnProperty.call(v1, $72)) {
                                                    $71[$72] = v1[$72];
                                                };
                                            };
                                            $71.pendingPage = Data_Maybe.Nothing.value;
                                            return $71;
                                        }));
                                    };
                                    return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Data_Unit.unit);
                                })())(function () {
                                    return Data_Lens_Getter.viewOn(moneroD)(Thermite._performAction(Data_Lens_Internal_Forget.strongForget))(PageAction.create(MoneroDAction.create(new Spec_Page_MoneroD.GotStatus(v.value0))))(props)(state);
                                });
                            });
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Monad_Eff_Class.liftEff(Control_Monad_Free_Trans.monadEffFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadEffAff))(Control_Monad_Eff_Console.warn("Got service status for non-pending service: " + Data_Show.show(System_SystemD_Status.showSystemDStatus)(v.value0)));
                        };
                    };
                    throw new Error("Failed pattern match at Spec line 136, column 9 - line 148, column 90: " + [ v.constructor.name ]);
                };
                return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff))(Data_Unit.unit);
            };
        };
    };
    var render = function (dispatch) {
        return function (props) {
            return function (v) {
                return function (children) {
                    var template = function (content) {
                        return [ MaterialUI_MuiThemeProvider.muiThemeProvider({
                            theme: MaterialUI_MuiThemeProvider.createMuiTheme(Data_Unit.unit)
                        })(React_DOM.div([  ])(content)) ];
                    };
                    return [ MaterialUI_AppBar.appBar(Data_Record_Class.srInst())({})([ MaterialUI_Toolbar.toolbar(Data_Record_Class.srInst())({})([ MaterialUI_Typography.typography(Data_Record_Class.srInst())({
                        type: MaterialUI_Typography.title, 
                        color: MaterialUI_Typography.inheritColor, 
                        style: MaterialUI_Types.createStyles({
                            flex: 1
                        })
                    })([ React_DOM.text("Monerodo") ]), MaterialUI_Button.button(Data_Record_Class.srInst())({
                        color: MaterialUI_Button.contrast, 
                        onTouchTap: Control_Monad_Eff_Uncurried.mkEffFn1(function (v1) {
                            return dispatch(new NavAction(ClickedMoneroD.value));
                        }), 
                        disabled: (function () {
                            if (v.currentPage instanceof MoneroD) {
                                return true;
                            };
                            return false;
                        })()
                    })([ React_DOM.text("monerod") ]), MaterialUI_Button.button(Data_Record_Class.srInst())({
                        color: MaterialUI_Button.contrast, 
                        onTouchTap: Control_Monad_Eff_Uncurried.mkEffFn1(function (v1) {
                            return dispatch(new NavAction(ClickedXmrStak.value));
                        }), 
                        disabled: (function () {
                            if (v.currentPage instanceof XmrStak) {
                                return true;
                            };
                            return false;
                        })()
                    })([ React_DOM.text("xmr-stak") ]) ]) ]), MaterialUI_Paper.paper(Data_Record_Class.srInst())({
                        style: MaterialUI_Types.createStyles({
                            marginTop: "5em", 
                            marginLeft: "auto", 
                            marginRight: "auto", 
                            padding: "1em"
                        })
                    })((function () {
                        if (v.currentPage instanceof MoneroD) {
                            return Data_Lens_Getter.viewOn(moneroD)(Thermite._render(Data_Lens_Internal_Forget.strongForget))(dispatch)(props)(v)(children);
                        };
                        if (v.currentPage instanceof XmrStak) {
                            return Data_Lens_Getter.viewOn(xmrStak)(Thermite._render(Data_Lens_Internal_Forget.strongForget))(dispatch)(props)(v)(children);
                        };
                        throw new Error("Failed pattern match at Spec line 183, column 13 - line 185, column 76: " + [ v.currentPage.constructor.name ]);
                    })()) ];
                };
            };
        };
    };
    return Thermite.simpleSpec(Data_Semigroup.append(Data_Semigroup.semigroupFn(Data_Semigroup.semigroupFn(Data_Semigroup.semigroupFn(Control_Monad_Free_Trans.semigroupFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff)(Data_Semigroup.semigroupUnit)))))(performAction)(Data_Semigroup.append(Data_Semigroup.semigroupFn(Data_Semigroup.semigroupFn(Data_Semigroup.semigroupFn(Control_Monad_Free_Trans.semigroupFreeT(Control_Coroutine.functorCoTransform)(Control_Monad_Aff.monadAff)(Data_Semigroup.semigroupUnit)))))(Data_Lens_Getter.viewOn(moneroD)(Thermite._performAction(Data_Lens_Internal_Forget.strongForget)))(Data_Lens_Getter.viewOn(xmrStak)(Thermite._performAction(Data_Lens_Internal_Forget.strongForget)))))(render);
})();
var main = function __do() {
    MaterialUI_InjectTapEvent.injectTapEvent();
    var v = DOM_HTML.window();
    var v1 = Queue.newQueue();
    var v2 = Queue.newQueue();
    var v3 = Queue.newQueue();
    Electron_Renderer.registerAsyncHandler({
        channel: Client_Constants.controlOutput, 
        handle: function (v4) {
            var v5 = Data_Argonaut_Decode_Class.decodeJson(Types_DBus.decodeJsonControlOutput)(v4.message);
            if (v5 instanceof Data_Either.Left) {
                return Control_Monad_Eff_Console.warn("Couldn't decode electron ipc message: " + Data_Show.show(Data_Show.showString)(v5.value0));
            };
            if (v5 instanceof Data_Either.Right) {
                return Queue.putQueue(v1)(new IpcAction(new Types_DBus.ControlOutput(v5.value0)));
            };
            throw new Error("Failed pattern match at Spec line 214, column 9 - line 217, column 64: " + [ v5.constructor.name ]);
        }
    })();
    Electron_Renderer.registerAsyncHandler({
        channel: Client_Constants.signalOutput, 
        handle: function (v4) {
            var v5 = Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeArray(Types_DBus.decodeJsonSignalOutput))(v4.message);
            if (v5 instanceof Data_Either.Left) {
                return Control_Monad_Eff_Console.warn("Couldn't decode electron ipc signal: " + (Data_Show.show(Data_Show.showString)(v5.value0) + (", " + Data_Show.show(Data_Argonaut_Core.showJson)(v4.message))));
            };
            if (v5 instanceof Data_Either.Right) {
                return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(function (x) {
                    return Queue.putQueue(v2)(new IpcAction(new Types_DBus.SignalOutput(x)));
                })(v5.value0);
            };
            throw new Error("Failed pattern match at Spec line 223, column 9 - line 226, column 83: " + [ v5.constructor.name ]);
        }
    })();
    Electron_Renderer.registerAsyncHandler({
        channel: Client_Constants.envOutput, 
        handle: function (v4) {
            var v5 = Data_Argonaut_Decode_Class.decodeJson(Types_Env.decodeJsonEnvData)(v4.message);
            if (v5 instanceof Data_Either.Left) {
                return Control_Monad_Eff_Console.warn("Couldn't decode electron ipc env: " + Data_Show.show(Data_Show.showString)(v5.value0));
            };
            if (v5 instanceof Data_Either.Right) {
                return Queue.putQueue(v3)(v5.value0);
            };
            throw new Error("Failed pattern match at Spec line 232, column 9 - line 235, column 32: " + [ v5.constructor.name ]);
        }
    })();
    Electron_Renderer.send({
        channel: Client_Constants.envOutput, 
        message: Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonUnit)(Data_Unit.unit)
    })();
    return Queue.onQueue(v3)(function (env) {
        return function __do() {
            var v4 = Control_Monad_Eff_Exception["try"]((function () {
                var v4 = Thermite.createReactSpec(spec)(initialState(env));
                var reactSpec$prime = (function () {
                    var $114 = {};
                    for (var $115 in v4.spec) {
                        if ({}.hasOwnProperty.call(v4.spec, $115)) {
                            $114[$115] = v4["spec"][$115];
                        };
                    };
                    $114.componentDidMount = function ($$this) {
                        return function __do() {
                            Queue.onQueue(v2)(v4.dispatcher($$this))();
                            Queue.onQueue(v1)(v4.dispatcher($$this))();
                            return v4.spec.componentDidMount($$this)();
                        };
                    };
                    return $114;
                })();
                var component = React.createClass(reactSpec$prime);
                return function __do() {
                    Control_Bind.bindFlipped(Control_Monad_Eff.bindEff)(Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(function ($130) {
                        return ReactDOM.render(React.createFactory(component)(Data_Unit.unit))(DOM_HTML_Types.htmlElementToElement($130));
                    }))(Control_Bind.bindFlipped(Control_Monad_Eff.bindEff)(DOM_HTML_Document.body)(DOM_HTML_Window.document(v)))();
                    var resolve = function (v5) {
                        if (v5 instanceof Data_Either.Left) {
                            return Control_Monad_Eff_Console.warn(Data_Show.show(Control_Monad_Eff_Exception.showError)(v5.value0));
                        };
                        if (v5 instanceof Data_Either.Right) {
                            return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
                        };
                        throw new Error("Failed pattern match at Spec line 255, column 11 - line 255, column 43: " + [ v5.constructor.name ]);
                    };
                    return Control_Monad_Aff.runAff_(resolve)(Control_Parallel_Class.sequential(Control_Monad_Aff.parallelAff)(Control_Apply.apply(Control_Monad_Aff.applyParAff)(Data_Functor.map(Control_Monad_Aff.functorParAff)(Data_Tuple.Tuple.create)(Control_Parallel_Class.parallel(Control_Monad_Aff.parallelAff)(Control_Monad_Rec_Class.forever(Control_Monad_Aff.monadRecAff)(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Electron_Renderer.send({
                        channel: Client_Constants.signalOutput, 
                        message: Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonUnit)(Data_Unit.unit)
                    })))(function () {
                        return Control_Monad_Aff.delay(300.0);
                    })))))(Control_Parallel_Class.parallel(Control_Monad_Aff.parallelAff)(Control_Monad_Rec_Class.forever(Control_Monad_Aff.monadRecAff)(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Electron_Renderer.send({
                        channel: Client_Constants.controlInput, 
                        message: Data_Argonaut_Encode_Class.encodeJson(Types_DBus.encodeJsonControlInput)(Types_DBus.GetServiceState.create(new Data_Maybe.Just(Types_DBus.ServiceMoneroD.value)))
                    })))(function () {
                        return Control_Monad_Aff.delay(1000.0);
                    }))))))();
                };
            })())();
            if (v4 instanceof Data_Either.Left) {
                return Control_Monad_Eff_Console.warn(Data_Show.show(Control_Monad_Eff_Exception.showError)(v4.value0))();
            };
            if (v4 instanceof Data_Either.Right) {
                return Data_Unit.unit;
            };
            throw new Error("Failed pattern match at Spec line 272, column 5 - line 274, column 23: " + [ v4.constructor.name ]);
        };
    })();
};
module.exports = {
    NavAction: NavAction, 
    IpcAction: IpcAction, 
    PageAction: PageAction, 
    ClickedMoneroD: ClickedMoneroD, 
    ClickedXmrStak: ClickedXmrStak, 
    MoneroD: MoneroD, 
    XmrStak: XmrStak, 
    MoneroDAction: MoneroDAction, 
    XmrStakAction: XmrStakAction, 
    MoneroDPage: MoneroDPage, 
    XmrStakPage: XmrStakPage, 
    _MoneroD: _MoneroD, 
    _MoneroDAction: _MoneroDAction, 
    _PageAction: _PageAction, 
    _XmrStak: _XmrStak, 
    _XmrStakAction: _XmrStakAction, 
    _currentPage: _currentPage, 
    initialState: initialState, 
    main: main, 
    spec: spec
};

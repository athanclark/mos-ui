// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var spec$prime = function (getInitialState) {
    return function (renderFn) {
        return {
            render: renderFn, 
            displayName: "", 
            getInitialState: getInitialState, 
            componentWillMount: function (v) {
                return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
            }, 
            componentDidMount: function (v) {
                return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
            }, 
            componentWillReceiveProps: function (v) {
                return function (v1) {
                    return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
                };
            }, 
            shouldComponentUpdate: function (v) {
                return function (v1) {
                    return function (v2) {
                        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(true);
                    };
                };
            }, 
            componentWillUpdate: function (v) {
                return function (v1) {
                    return function (v2) {
                        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
                    };
                };
            }, 
            componentDidUpdate: function (v) {
                return function (v1) {
                    return function (v2) {
                        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
                    };
                };
            }, 
            componentWillUnmount: function (v) {
                return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
            }
        };
    };
};
var spec = function (state) {
    return spec$prime(function (v) {
        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(state);
    });
};
var readRef = function ($$this) {
    return function (name) {
        return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign.readRefImpl($$this)(name));
    };
};
var forceUpdateCb = function ($$this) {
    return function (m) {
        return Control_Monad_Eff_Uncurried.runEffFn2($foreign.forceUpdateCbImpl)($$this)(m);
    };
};
var forceUpdate = function ($$this) {
    return forceUpdateCb($$this)(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit));
};
var createClassStateless = Unsafe_Coerce.unsafeCoerce;
var createClassStateless$prime = function (k) {
    return createClassStateless(function (props) {
        return k(props)($foreign.childrenToArray((Unsafe_Coerce.unsafeCoerce(props)).children));
    });
};
module.exports = {
    createClassStateless: createClassStateless, 
    "createClassStateless'": createClassStateless$prime, 
    forceUpdate: forceUpdate, 
    forceUpdateCb: forceUpdateCb, 
    readRef: readRef, 
    spec: spec, 
    "spec'": spec$prime, 
    childrenToArray: $foreign.childrenToArray, 
    createClass: $foreign.createClass, 
    createElement: $foreign.createElement, 
    createElementDynamic: $foreign.createElementDynamic, 
    createElementTagName: $foreign.createElementTagName, 
    createElementTagNameDynamic: $foreign.createElementTagNameDynamic, 
    createFactory: $foreign.createFactory, 
    getChildren: $foreign.getChildren, 
    getProps: $foreign.getProps, 
    getRefs: $foreign.getRefs, 
    handle: $foreign.handle, 
    preventDefault: $foreign.preventDefault, 
    readState: $foreign.readState, 
    stopPropagation: $foreign.stopPropagation, 
    transformState: $foreign.transformState, 
    writeRef: $foreign.writeRef, 
    writeState: $foreign.writeState, 
    writeStateWithCallback: $foreign.writeStateWithCallback
};
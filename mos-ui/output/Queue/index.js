// Generated by purs version 0.11.6
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Traversable = require("../Data.Traversable");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_Channel = require("../Signal.Channel");
var Queue = function (x) {
    return x;
};
var takeQueue = function (v) {
    return function __do() {
        var v1 = Control_Monad_Eff_Ref.readRef(v.pending)();
        Control_Monad_Eff_Ref.writeRef(v.pending)([  ])();
        return v1;
    };
};
var readQueue = function (v) {
    return Control_Monad_Eff_Ref.readRef(v.pending);
};
var putManyQueue = function (v) {
    return function (xs) {
        return function __do() {
            Control_Monad_Eff_Ref.modifyRef(v.pending)(function (ys) {
                return Data_Semigroup.append(Data_Semigroup.semigroupArray)(ys)(xs);
            })();
            return Signal_Channel.send(v.chan)(Data_Unit.unit)();
        };
    };
};
var putQueue = function (q) {
    return function (x) {
        return putManyQueue(q)([ x ]);
    };
};
var onQueue = function (v) {
    return function (f) {
        return Signal.runSignal((function () {
            var go = function __do() {
                var v1 = Control_Monad_Eff_Ref.readRef(v.pending)();
                Control_Monad_Eff_Ref.writeRef(v.pending)([  ])();
                return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(f)(v1)();
            };
            return Signal.sampleOn(Signal_Channel.subscribe(v.chan))(Signal.constant(go));
        })());
    };
};
var newQueue = function __do() {
    var v = Signal_Channel.channel(Data_Unit.unit)();
    var v1 = Control_Monad_Eff_Ref.newRef([  ])();
    return {
        pending: v1, 
        chan: v
    };
};
module.exports = {
    newQueue: newQueue, 
    onQueue: onQueue, 
    putManyQueue: putManyQueue, 
    putQueue: putQueue, 
    readQueue: readQueue, 
    takeQueue: takeQueue
};
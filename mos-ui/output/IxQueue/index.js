// Generated by purs version 0.11.6
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_List_Types = require("../Data.List.Types");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Queue = require("../Queue");
var Signal_Channel = require("../Signal.Channel");
var IxQueue = function (x) {
    return x;
};
var putManyIxQueue = function (dictOrd) {
    return function (v) {
        return function (k) {
            return function (xs) {
                return function __do() {
                    var v1 = Control_Monad_Eff_Ref.readRef(v)();
                    var v2 = Data_Map.lookup(dictOrd)(k)(v1);
                    if (v2 instanceof Data_Maybe.Nothing) {
                        var v3 = Queue.newQueue();
                        Control_Monad_Eff_Ref.writeRef(v)(Data_Map.insert(dictOrd)(k)(v3)(v1))();
                        return Queue.putManyQueue(v3)(xs)();
                    };
                    if (v2 instanceof Data_Maybe.Just) {
                        return Queue.putManyQueue(v2.value0)(xs)();
                    };
                    throw new Error("Failed pattern match at IxQueue line 47, column 3 - line 52, column 32: " + [ v2.constructor.name ]);
                };
            };
        };
    };
};
var putIxQueue = function (dictOrd) {
    return function (q) {
        return function (k) {
            return function (x) {
                return putManyIxQueue(dictOrd)(q)(k)([ x ]);
            };
        };
    };
};
var onIxQueue = function (dictOrd) {
    return function (v) {
        return function (k) {
            return function (f) {
                return function __do() {
                    var v1 = Control_Monad_Eff_Ref.readRef(v)();
                    var v2 = Data_Map.lookup(dictOrd)(k)(v1);
                    if (v2 instanceof Data_Maybe.Nothing) {
                        var v3 = Queue.newQueue();
                        Control_Monad_Eff_Ref.writeRef(v)(Data_Map.insert(dictOrd)(k)(v3)(v1))();
                        return Queue.onQueue(v3)(f)();
                    };
                    if (v2 instanceof Data_Maybe.Just) {
                        return Queue.onQueue(v2.value0)(f)();
                    };
                    throw new Error("Failed pattern match at IxQueue line 123, column 3 - line 128, column 26: " + [ v2.constructor.name ]);
                };
            };
        };
    };
};
var newIxQueue = Data_Functor.map(Control_Monad_Eff.functorEff)(IxQueue)(Control_Monad_Eff_Ref.newRef(Data_Map.empty));
var injectIxQueue = function (dictOrd) {
    return function (v) {
        return function (k) {
            return function (q) {
                return function __do() {
                    var v1 = Control_Monad_Eff_Ref.readRef(v)();
                    var v2 = Data_Map.lookup(dictOrd)(k)(v1);
                    if (v2 instanceof Data_Maybe.Nothing) {
                        return Control_Monad_Eff_Ref.writeRef(v)(Data_Map.insert(dictOrd)(k)(q)(v1))();
                    };
                    if (v2 instanceof Data_Maybe.Just) {
                        var v3 = Queue.takeQueue(v2.value0)();
                        Control_Monad_Eff_Ref.writeRef(v)(Data_Map.insert(dictOrd)(k)(q)(v1))();
                        return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(Queue.putQueue(q))(v3)();
                    };
                    throw new Error("Failed pattern match at IxQueue line 90, column 3 - line 96, column 32: " + [ v2.constructor.name ]);
                };
            };
        };
    };
};
var touchIxQueue = function (dictOrd) {
    return function (queue) {
        return function (k) {
            return function __do() {
                var v = Queue.newQueue();
                return injectIxQueue(dictOrd)(queue)(k)(v)();
            };
        };
    };
};
var delIxQueue = function (dictOrd) {
    return function (v) {
        return function (k) {
            return Control_Monad_Eff_Ref.modifyRef(v)(Data_Map["delete"](dictOrd)(k));
        };
    };
};
var broadcastManyIxQueue = function (dictOrd) {
    return function (v) {
        return function (xs) {
            return function __do() {
                var v1 = Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Map.keys)(Control_Monad_Eff_Ref.readRef(v))();
                return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_List_Types.foldableList)(function (k) {
                    return putManyIxQueue(dictOrd)(v)(k)(xs);
                })(v1)();
            };
        };
    };
};
var broadcastIxQueue = function (dictOrd) {
    return function (q) {
        return function (x) {
            return broadcastManyIxQueue(dictOrd)(q)([ x ]);
        };
    };
};
module.exports = {
    broadcastIxQueue: broadcastIxQueue, 
    broadcastManyIxQueue: broadcastManyIxQueue, 
    delIxQueue: delIxQueue, 
    injectIxQueue: injectIxQueue, 
    newIxQueue: newIxQueue, 
    onIxQueue: onIxQueue, 
    putIxQueue: putIxQueue, 
    putManyIxQueue: putManyIxQueue, 
    touchIxQueue: touchIxQueue
};
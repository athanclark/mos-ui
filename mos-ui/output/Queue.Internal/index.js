// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Traversable = require("../Data.Traversable");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Queue = function (x) {
    return x;
};
var writeOnly = function (v) {
    return v;
};
var takeQueue = function (v) {
    return function __do() {
        var v1 = Control_Monad_Eff_Ref.readRef(v)();
        if (v1 instanceof Data_Either.Left) {
            Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Left([  ]))();
            return v1.value0;
        };
        if (v1 instanceof Data_Either.Right) {
            return [  ];
        };
        throw new Error("Failed pattern match at Queue.Internal line 98, column 3 - line 102, column 23: " + [ v1.constructor.name ]);
    };
};
var readQueue = function (v) {
    return function __do() {
        var v1 = Control_Monad_Eff_Ref.readRef(v)();
        if (v1 instanceof Data_Either.Left) {
            return v1.value0;
        };
        if (v1 instanceof Data_Either.Right) {
            return [  ];
        };
        throw new Error("Failed pattern match at Queue.Internal line 90, column 3 - line 92, column 23: " + [ v1.constructor.name ]);
    };
};
var readOnly = function (v) {
    return v;
};
var putManyQueue = function (v) {
    return function (xs) {
        return function __do() {
            var v1 = Control_Monad_Eff_Ref.readRef(v)();
            if (v1 instanceof Data_Either.Left) {
                return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Left(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v1.value0)(xs)))();
            };
            if (v1 instanceof Data_Either.Right) {
                return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(function (x) {
                    return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(function (f) {
                        return f(x);
                    })(v1.value0);
                })(xs)();
            };
            throw new Error("Failed pattern match at Queue.Internal line 49, column 3 - line 51, column 74: " + [ v1.constructor.name ]);
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
        return function __do() {
            var v1 = Control_Monad_Eff_Ref.readRef(v)();
            if (v1 instanceof Data_Either.Left) {
                Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(f)(v1.value0)();
                return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Right([ f ]))();
            };
            if (v1 instanceof Data_Either.Right) {
                return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Right(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v1.value0)([ f ])))();
            };
            throw new Error("Failed pattern match at Queue.Internal line 57, column 3 - line 62, column 47: " + [ v1.constructor.name ]);
        };
    };
};
var newQueue = Data_Functor.map(Control_Monad_Eff.functorEff)(Queue)(Control_Monad_Eff_Ref.newRef(new Data_Either.Left([  ])));
var delQueue = function (v) {
    return function __do() {
        var v1 = Control_Monad_Eff_Ref.readRef(v)();
        if (v1 instanceof Data_Either.Left) {
            return Data_Unit.unit;
        };
        if (v1 instanceof Data_Either.Right) {
            return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Left([  ]))();
        };
        throw new Error("Failed pattern match at Queue.Internal line 109, column 3 - line 111, column 39: " + [ v1.constructor.name ]);
    };
};
var onceQueue = function (v) {
    return function (f$prime) {
        return function __do() {
            var v1 = Control_Monad_Eff_Ref.newRef(false)();
            var f = function (x) {
                return function __do() {
                    var v2 = Control_Monad_Eff_Ref.readRef(v1)();
                    Control_Applicative.unless(Control_Monad_Eff.applicativeEff)(v2)(f$prime(x))();
                    Control_Monad_Eff_Ref.writeRef(v1)(true)();
                    return delQueue(v)();
                };
            };
            var v2 = Control_Monad_Eff_Ref.readRef(v)();
            if (v2 instanceof Data_Either.Left) {
                var v3 = Data_Array.uncons(v2.value0);
                if (v3 instanceof Data_Maybe.Nothing) {
                    return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Right([ f ]))();
                };
                if (v3 instanceof Data_Maybe.Just) {
                    f(v3.value0.head)();
                    return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Left(v3.value0.tail))();
                };
                throw new Error("Failed pattern match at Queue.Internal line 77, column 7 - line 82, column 37: " + [ v3.constructor.name ]);
            };
            if (v2 instanceof Data_Either.Right) {
                return Control_Monad_Eff_Ref.writeRef(v)(new Data_Either.Right(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v2.value0)([ f ])))();
            };
            throw new Error("Failed pattern match at Queue.Internal line 75, column 3 - line 84, column 47: " + [ v2.constructor.name ]);
        };
    };
};
var allowWriting = function (v) {
    return v;
};
var allowReading = function (v) {
    return v;
};
module.exports = {
    Queue: Queue,
    newQueue: newQueue,
    readOnly: readOnly,
    allowWriting: allowWriting,
    writeOnly: writeOnly,
    allowReading: allowReading,
    putQueue: putQueue,
    putManyQueue: putManyQueue,
    onQueue: onQueue,
    onceQueue: onceQueue,
    readQueue: readQueue,
    takeQueue: takeQueue,
    delQueue: delQueue
};
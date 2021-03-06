// Generated by purs version 0.11.7
"use strict";
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_Trans_Control = require("../Control.Monad.Trans.Control");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Compose = require("../Data.Functor.Compose");
var Data_Identity = require("../Data.Identity");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var SingletonFunctor = function (Functor0, getSingleton) {
    this.Functor0 = Functor0;
    this.getSingleton = getSingleton;
};
var singletonFunctorWriterTStT = new SingletonFunctor(function () {
    return Control_Monad_Trans_Control.functorWriterTStT;
}, function (v) {
    return v.value1;
});
var singletonFunctorUnitFunction = new SingletonFunctor(function () {
    return Data_Functor.functorFn;
}, function (f) {
    return f(Data_Unit.unit);
});
var singletonFunctorTuple = new SingletonFunctor(function () {
    return Data_Tuple.functorTuple;
}, function (v) {
    return v.value1;
});
var singletonFunctorIdentity = new SingletonFunctor(function () {
    return Data_Identity.functorIdentity;
}, function (v) {
    return v;
});
var getSingleton = function (dict) {
    return dict.getSingleton;
};
var liftBaseWith_ = function (dictMonadBaseControl) {
    return function (dictSingletonFunctor) {
        return function (dictFunctor) {
            return function (f) {
                return Control_Monad_Trans_Control.liftBaseWith(dictMonadBaseControl)(function (runInBase) {
                    return f(function ($28) {
                        return Data_Functor.map(dictFunctor)(getSingleton(dictSingletonFunctor))(runInBase($28));
                    });
                });
            };
        };
    };
};
var liftWith_ = function (dictMonadTransControl) {
    return function (dictSingletonFunctor) {
        return function (dictMonad) {
            return function (f) {
                return Control_Monad_Trans_Control.liftWith(dictMonadTransControl)(function (run) {
                    return f(function ($29) {
                        return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(getSingleton(dictSingletonFunctor))(run($29));
                    });
                });
            };
        };
    };
};
var singletonFunctorCompose = function (dictSingletonFunctor) {
    return function (dictSingletonFunctor1) {
        return new SingletonFunctor(function () {
            return Data_Functor_Compose.functorCompose(dictSingletonFunctor.Functor0())(dictSingletonFunctor1.Functor0());
        }, function (v) {
            return getSingleton(dictSingletonFunctor1)(getSingleton(dictSingletonFunctor)(v));
        });
    };
};
var singletonFunctorFreeTStT = function (dictSingletonFunctor) {
    return function (dictSingletonFunctor1) {
        return function (dictMonadRec) {
            return new SingletonFunctor(function () {
                return Control_Monad_Trans_Control.functorFreeTStT(dictSingletonFunctor.Functor0())(dictMonadRec.Monad0());
            }, function (v) {
                if (v instanceof Data_Either.Left) {
                    return v.value0;
                };
                if (v instanceof Data_Either.Right) {
                    return getSingleton(singletonFunctorFreeTStT(dictSingletonFunctor)(dictSingletonFunctor1)(dictMonadRec))(getSingleton(dictSingletonFunctor1)(Control_Monad_Free_Trans.resume(dictSingletonFunctor.Functor0())(dictMonadRec)(getSingleton(dictSingletonFunctor)(v.value0))));
                };
                throw new Error("Failed pattern match at Data.Functor.Singleton line 46, column 31 - line 48, column 80: " + [ v.constructor.name ]);
            });
        };
    };
};
module.exports = {
    getSingleton: getSingleton,
    SingletonFunctor: SingletonFunctor,
    liftWith_: liftWith_,
    liftBaseWith_: liftBaseWith_,
    singletonFunctorIdentity: singletonFunctorIdentity,
    singletonFunctorCompose: singletonFunctorCompose,
    singletonFunctorTuple: singletonFunctorTuple,
    singletonFunctorUnitFunction: singletonFunctorUnitFunction,
    singletonFunctorWriterTStT: singletonFunctorWriterTStT,
    singletonFunctorFreeTStT: singletonFunctorFreeTStT
};

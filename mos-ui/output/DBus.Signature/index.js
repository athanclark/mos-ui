// Generated by purs version 0.11.6
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Uncurried = require("../Control.Monad.Eff.Uncurried");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Decimal = require("../Data.Decimal");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Foreign = require("../Data.Foreign");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Tuple_Native = require("../Data.Tuple.Native");
var Data_Unfoldable = require("../Data.Unfoldable");
var Prelude = require("../Prelude");
var Type_Proxy = require("../Type.Proxy");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Signature = function (x) {
    return x;
};
var DBusBoolean = (function () {
    function DBusBoolean() {

    };
    DBusBoolean.value = new DBusBoolean();
    return DBusBoolean;
})();
var DBusInt32 = (function () {
    function DBusInt32() {

    };
    DBusInt32.value = new DBusInt32();
    return DBusInt32;
})();
var DBusDouble = (function () {
    function DBusDouble() {

    };
    DBusDouble.value = new DBusDouble();
    return DBusDouble;
})();
var DBusString = (function () {
    function DBusString() {

    };
    DBusString.value = new DBusString();
    return DBusString;
})();
var DBusArray = (function () {
    function DBusArray(value0) {
        this.value0 = value0;
    };
    DBusArray.create = function (value0) {
        return new DBusArray(value0);
    };
    return DBusArray;
})();
var DBusDictionary = (function () {
    function DBusDictionary(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    DBusDictionary.create = function (value0) {
        return function (value1) {
            return new DBusDictionary(value0, value1);
        };
    };
    return DBusDictionary;
})();
var DBusStructure = (function () {
    function DBusStructure(value0) {
        this.value0 = value0;
    };
    DBusStructure.create = function (value0) {
        return new DBusStructure(value0);
    };
    return DBusStructure;
})();
var IsVariant = function (fromVariant, toVariant) {
    this.fromVariant = fromVariant;
    this.toVariant = toVariant;
};
var IsValue = function (IsVariant0, typeOf) {
    this.IsVariant0 = IsVariant0;
    this.typeOf = typeOf;
};
var IsKey = function (IsValue0, Ord1) {
    this.IsValue0 = IsValue0;
    this.Ord1 = Ord1;
};
var AutoMethod = function (types) {
    this.types = types;
};
var types = function (dict) {
    return dict.types;
};
var typeOf = function (dict) {
    return dict.typeOf;
};
var typeCode = function (t) {
    var getSignature = function (v) {
        return v;
    };
    return Signature((function () {
        if (t instanceof DBusBoolean) {
            return "b";
        };
        if (t instanceof DBusInt32) {
            return "i";
        };
        if (t instanceof DBusDouble) {
            return "d";
        };
        if (t instanceof DBusString) {
            return "s";
        };
        if (t instanceof DBusArray) {
            var v = typeCode(t.value0);
            return "a" + v;
        };
        if (t instanceof DBusDictionary) {
            var v1 = new Data_Tuple.Tuple(typeCode(t.value0), typeCode(t.value1));
            return "a{" + (v1.value0 + (v1.value1 + "}"));
        };
        if (t instanceof DBusStructure) {
            return Data_String.joinWith("")(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ "(" ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(function ($123) {
                return getSignature(typeCode($123));
            })(t.value0))([ ")" ])));
        };
        throw new Error("Failed pattern match at DBus.Signature line 48, column 26 - line 61, column 96: " + [ t.constructor.name ]);
    })());
};
var toVariant = function (dict) {
    return dict.toVariant;
};
var semigroupSignature = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var monoidSignature = new Data_Monoid.Monoid(function () {
    return semigroupSignature;
}, "");
var isVariantString = new IsVariant(function (x) {
    if (Data_Foreign.typeOf(Data_Foreign.toForeign(x)) === "string") {
        return new Data_Either.Right(Unsafe_Coerce.unsafeCoerce(x));
    };
    if (Data_Boolean.otherwise) {
        return new Data_Either.Left("Not a string");
    };
    throw new Error("Failed pattern match at DBus.Signature line 91, column 1 - line 91, column 45: " + [ x.constructor.name ]);
}, Unsafe_Coerce.unsafeCoerce);
var isVariantNumber = new IsVariant(function (x) {
    if (Data_Foreign.typeOf(Data_Foreign.toForeign(x)) === "number" && !Data_Decimal.isInteger(Unsafe_Coerce.unsafeCoerce(x))) {
        return new Data_Either.Right(Unsafe_Coerce.unsafeCoerce(x));
    };
    if (Data_Boolean.otherwise) {
        return new Data_Either.Left("Not a Number or not isInteger");
    };
    throw new Error("Failed pattern match at DBus.Signature line 85, column 1 - line 85, column 45: " + [ x.constructor.name ]);
}, Unsafe_Coerce.unsafeCoerce);
var isVariantInt = new IsVariant(function (x) {
    if (Data_Foreign.typeOf(Data_Foreign.toForeign(x)) === "number" && Data_Decimal.isInteger(Unsafe_Coerce.unsafeCoerce(x))) {
        return new Data_Either.Right(Unsafe_Coerce.unsafeCoerce(x));
    };
    if (Data_Boolean.otherwise) {
        return new Data_Either.Left("Not a Int or isInteger");
    };
    throw new Error("Failed pattern match at DBus.Signature line 79, column 1 - line 79, column 39: " + [ x.constructor.name ]);
}, Unsafe_Coerce.unsafeCoerce);
var isVariantBoolean = new IsVariant(function (x) {
    if (Data_Foreign.typeOf(Data_Foreign.toForeign(x)) === "boolean") {
        return new Data_Either.Right(Unsafe_Coerce.unsafeCoerce(x));
    };
    if (Data_Boolean.otherwise) {
        return new Data_Either.Left("Not a boolean");
    };
    throw new Error("Failed pattern match at DBus.Signature line 73, column 1 - line 73, column 47: " + [ x.constructor.name ]);
}, Unsafe_Coerce.unsafeCoerce);
var isVariantArray = function (dictIsVariant) {
    return new IsVariant(function (x) {
        if (Data_Foreign.isArray(Data_Foreign.toForeign(x))) {
            return new Data_Either.Right(Unsafe_Coerce.unsafeCoerce(x));
        };
        if (Data_Boolean.otherwise) {
            return new Data_Either.Left("Not an array");
        };
        throw new Error("Failed pattern match at DBus.Signature line 97, column 1 - line 97, column 62: " + [ x.constructor.name ]);
    }, Unsafe_Coerce.unsafeCoerce);
};
var isValueString = new IsValue(function () {
    return isVariantString;
}, function (v) {
    return DBusString.value;
});
var isValueNumber = new IsValue(function () {
    return isVariantNumber;
}, function (v) {
    return DBusDouble.value;
});
var isValueInt = new IsValue(function () {
    return isVariantInt;
}, function (v) {
    return DBusInt32.value;
});
var isValueBoolean = new IsValue(function () {
    return isVariantBoolean;
}, function (v) {
    return DBusBoolean.value;
});
var isValueArray = function (dictIsValue) {
    return new IsValue(function () {
        return isVariantArray(dictIsValue.IsVariant0());
    }, function (v) {
        return new DBusArray(typeOf(dictIsValue)(Type_Proxy["Proxy"].value));
    });
};
var isKeyString = new IsKey(function () {
    return isValueString;
}, function () {
    return Data_Ord.ordString;
});
var isKeyNumber = new IsKey(function () {
    return isValueNumber;
}, function () {
    return Data_Ord.ordNumber;
});
var isKeyInt = new IsKey(function () {
    return isValueInt;
}, function () {
    return Data_Ord.ordInt;
});
var isKeyBoolean = new IsKey(function () {
    return isValueBoolean;
}, function () {
    return Data_Ord.ordBoolean;
});
var fromVariant = function (dict) {
    return dict.fromVariant;
};
var isVariantMap = function (dictIsKey) {
    return function (dictIsVariant) {
        return function (dictOrd) {
            return new IsVariant(function (xs) {
                if (Data_Foreign.isArray(Data_Foreign.toForeign(xs))) {
                    return Control_Bind.bind(Data_Either.bindEither)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(function (kv) {
                        var $76 = Data_Foreign.isArray(Data_Foreign.toForeign(kv));
                        if ($76) {
                            return Control_Bind.bind(Data_Either.bindEither)((function () {
                                var v = Data_Array.head(Unsafe_Coerce.unsafeCoerce(kv));
                                if (v instanceof Data_Maybe.Nothing) {
                                    return new Data_Either.Left("no first element");
                                };
                                if (v instanceof Data_Maybe.Just) {
                                    return Control_Applicative.pure(Data_Either.applicativeEither)(v.value0);
                                };
                                throw new Error("Failed pattern match at DBus.Signature line 125, column 57 - line 127, column 75: " + [ v.constructor.name ]);
                            })())(function (v) {
                                return Control_Bind.bind(Data_Either.bindEither)((function () {
                                    var v1 = Data_Array.head(v.tail);
                                    if (v1 instanceof Data_Maybe.Nothing) {
                                        return new Data_Either.Left("no second element");
                                    };
                                    if (v1 instanceof Data_Maybe.Just) {
                                        return Control_Applicative.pure(Data_Either.applicativeEither)(v1.value0);
                                    };
                                    throw new Error("Failed pattern match at DBus.Signature line 128, column 52 - line 130, column 70: " + [ v1.constructor.name ]);
                                })())(function (v1) {
                                    return Control_Bind.bind(Data_Either.bindEither)(fromVariant((dictIsKey.IsValue0()).IsVariant0())(v.head))(function (v2) {
                                        return Control_Bind.bind(Data_Either.bindEither)(fromVariant(dictIsVariant)(v1.head))(function (v3) {
                                            return Control_Applicative.pure(Data_Either.applicativeEither)(new Data_Tuple.Tuple(v2, v3));
                                        });
                                    });
                                });
                            });
                        };
                        return new Data_Either.Left("Not an array - map elems");
                    })(Unsafe_Coerce.unsafeCoerce(xs)))(function (v) {
                        return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Map.fromFoldable(dictOrd)(Data_Foldable.foldableArray)(v));
                    });
                };
                if (Data_Boolean.otherwise) {
                    return new Data_Either.Left("Not an array - map");
                };
                throw new Error("Failed pattern match at DBus.Signature line 118, column 1 - line 118, column 78: " + [ xs.constructor.name ]);
            }, function (xs) {
                return Unsafe_Coerce.unsafeCoerce(Data_Functor.map(Data_Functor.functorArray)(function (v) {
                    return Unsafe_Coerce.unsafeCoerce([ toVariant((dictIsKey.IsValue0()).IsVariant0())(v.value0), toVariant(dictIsVariant)(v.value1) ]);
                })(Data_Map.toUnfoldable(Data_Unfoldable.unfoldableArray)(xs)));
            });
        };
    };
};
var isValueMap = function (dictIsKey) {
    return function (dictIsValue) {
        return function (dictOrd) {
            return new IsValue(function () {
                return isVariantMap(dictIsKey)(dictIsValue.IsVariant0())(dictOrd);
            }, function (v) {
                return new DBusDictionary(typeOf(dictIsKey.IsValue0())(Type_Proxy["Proxy"].value), typeOf(dictIsValue)(Type_Proxy["Proxy"].value));
            });
        };
    };
};
var isVariantTuple = function (dictIsVariant) {
    return function (dictIsVariant1) {
        return new IsVariant(function (xs) {
            if (Data_Foreign.isArray(Data_Foreign.toForeign(xs))) {
                return Control_Bind.bind(Data_Either.bindEither)((function () {
                    var v = Data_Array.head(Unsafe_Coerce.unsafeCoerce(xs));
                    if (v instanceof Data_Maybe.Nothing) {
                        return new Data_Either.Left("no first element");
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure(Data_Either.applicativeEither)(v.value0);
                    };
                    throw new Error("Failed pattern match at DBus.Signature line 107, column 27 - line 109, column 45: " + [ v.constructor.name ]);
                })())(function (v) {
                    return Control_Bind.bind(Data_Either.bindEither)((function () {
                        var v1 = Data_Array.head(v.tail);
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return new Data_Either.Left("no second element");
                        };
                        if (v1 instanceof Data_Maybe.Just) {
                            return Control_Applicative.pure(Data_Either.applicativeEither)(v1.value0);
                        };
                        throw new Error("Failed pattern match at DBus.Signature line 110, column 22 - line 112, column 40: " + [ v1.constructor.name ]);
                    })())(function (v1) {
                        return Control_Bind.bind(Data_Either.bindEither)(fromVariant(dictIsVariant)(v.head))(function (v2) {
                            return Control_Bind.bind(Data_Either.bindEither)(fromVariant(dictIsVariant1)(v1.head))(function (v3) {
                                return Control_Applicative.pure(Data_Either.applicativeEither)(new Data_Tuple.Tuple(v2, v3));
                            });
                        });
                    });
                });
            };
            if (Data_Boolean.otherwise) {
                return new Data_Either.Left("Not an array - tuple");
            };
            throw new Error("Failed pattern match at DBus.Signature line 103, column 1 - line 103, column 79: " + [ xs.constructor.name ]);
        }, function (v) {
            return Unsafe_Coerce.unsafeCoerce([ toVariant(dictIsVariant)(v.value0), toVariant(dictIsVariant1)(v.value1) ]);
        });
    };
};
var isValueTuple = function (dictIsValue) {
    return function (dictIsValue1) {
        return new IsValue(function () {
            return isVariantTuple(dictIsValue.IsVariant0())(dictIsValue1.IsVariant0());
        }, function (v) {
            return new DBusStructure([ typeOf(dictIsValue)(Type_Proxy["Proxy"].value), typeOf(dictIsValue1)(Type_Proxy["Proxy"].value) ]);
        });
    };
};
var condenseArity = function (xs) {
    var getSignature = function (v) {
        return v;
    };
    var v = Data_Array.uncons(xs);
    if (v instanceof Data_Maybe.Nothing) {
        return "";
    };
    if (v instanceof Data_Maybe.Just) {
        if (Data_Array.length(v.value0.tail) === 0) {
            return v.value0.head;
        };
        if (Data_Boolean.otherwise) {
            return Signature(Data_String.joinWith("")(Data_Functor.map(Data_Functor.functorArray)(getSignature)(xs)));
        };
    };
    throw new Error("Failed pattern match at DBus.Signature line 184, column 20 - line 188, column 72: " + [ v.constructor.name ]);
};
var autoMethodEffFn1 = function (dictIsValue) {
    return function (dictIsValue1) {
        return new AutoMethod(function (v) {
            return new Data_Tuple.Tuple([ typeCode(typeOf(dictIsValue)(Type_Proxy["Proxy"].value)) ], typeCode(typeOf(dictIsValue1)(Type_Proxy["Proxy"].value)));
        });
    };
};
var autoMethodEff = function (dictIsValue) {
    return new AutoMethod(function (v) {
        return Data_Tuple.Tuple.create([  ])(typeCode(typeOf(dictIsValue)(Type_Proxy["Proxy"].value)));
    });
};
var autoMethod = function (dictAutoMethod) {
    return function (v) {
        return function (fn) {
            var v1 = types(dictAutoMethod)(Type_Proxy["Proxy"].value);
            return {
                desc: Data_Tuple_Native.t4(condenseArity(v1.value0))(v1.value1)(v.inputDesc)(v.outputDesc), 
                func: Unsafe_Coerce.unsafeCoerce(fn)
            };
        };
    };
};
module.exports = {
    AutoMethod: AutoMethod, 
    IsKey: IsKey, 
    IsValue: IsValue, 
    IsVariant: IsVariant, 
    autoMethod: autoMethod, 
    fromVariant: fromVariant, 
    toVariant: toVariant, 
    typeCode: typeCode, 
    typeOf: typeOf, 
    types: types, 
    semigroupSignature: semigroupSignature, 
    monoidSignature: monoidSignature, 
    isVariantBoolean: isVariantBoolean, 
    isVariantInt: isVariantInt, 
    isVariantNumber: isVariantNumber, 
    isVariantString: isVariantString, 
    isVariantArray: isVariantArray, 
    isVariantTuple: isVariantTuple, 
    isVariantMap: isVariantMap, 
    isValueBoolean: isValueBoolean, 
    isValueInt: isValueInt, 
    isValueNumber: isValueNumber, 
    isValueString: isValueString, 
    isValueArray: isValueArray, 
    isValueMap: isValueMap, 
    isValueTuple: isValueTuple, 
    isKeyBoolean: isKeyBoolean, 
    isKeyInt: isKeyInt, 
    isKeyNumber: isKeyNumber, 
    isKeyString: isKeyString, 
    autoMethodEff: autoMethodEff, 
    autoMethodEffFn1: autoMethodEffFn1
};
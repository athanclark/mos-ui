// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Data_Argonaut = require("../Data.Argonaut");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class");
var Data_Argonaut_Decode_Combinators = require("../Data.Argonaut.Decode.Combinators");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Argonaut_Encode_Combinators = require("../Data.Argonaut.Encode.Combinators");
var Data_Argonaut_JCursor = require("../Data.Argonaut.JCursor");
var Data_Boolean = require("../Data.Boolean");
var Data_DateTime = require("../Data.DateTime");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_JSDate = require("../Data.JSDate");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Loaded = (function () {
    function Loaded() {

    };
    Loaded.value = new Loaded();
    return Loaded;
})();
var NotFound = (function () {
    function NotFound() {

    };
    NotFound.value = new NotFound();
    return NotFound;
})();
var Active = (function () {
    function Active() {

    };
    Active.value = new Active();
    return Active;
})();
var Failed = (function () {
    function Failed() {

    };
    Failed.value = new Failed();
    return Failed;
})();
var Inactive = (function () {
    function Inactive() {

    };
    Inactive.value = new Inactive();
    return Inactive;
})();
var SystemDStatus = function (x) {
    return x;
};
var encodeJsonLoadedState = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    if (v instanceof Loaded) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("loaded");
    };
    if (v instanceof NotFound) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("not-found");
    };
    throw new Error("Failed pattern match at System.SystemD.Status line 15, column 1 - line 15, column 57: " + [ v.constructor.name ]);
});
var encodeJsonActiveState = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    if (v instanceof Active) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("active");
    };
    if (v instanceof Failed) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("failed");
    };
    if (v instanceof Inactive) {
        return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("inactive");
    };
    throw new Error("Failed pattern match at System.SystemD.Status line 32, column 1 - line 32, column 57: " + [ v.constructor.name ]);
});
var encodeJsonSystemDStatus = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    return Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonJString)("name")(v.name))(Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonJString)("description")(v.description))(Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(encodeJsonLoadedState)("loadedState")(v.loadedState))(Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonJString)("loadedStateExtra")(v.loadedStateExtra))(Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(encodeJsonActiveState)("activeState")(v.activeState))(Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonMaybe(Data_Argonaut_Encode_Class.encodeJsonJString))("activeStateSince")(Data_Functor.map(Data_Maybe.functorMaybe)(function (x) {
        return Control_Monad_Eff_Unsafe.unsafePerformEff(Data_JSDate.toISOString(Data_JSDate.fromDateTime(x)));
    })(v.activeStateSince)))(Data_Argonaut_Core.jsonEmptyObject))))));
});
var showSystemDStatus = new Data_Show.Show(function (x) {
    return Data_Show.show(Data_Argonaut_Core.showJson)(Data_Argonaut_Encode_Class.encodeJson(encodeJsonSystemDStatus)(x));
});
var decodeJsonLoadedState = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeJsonString)(json))(function (v) {
        if (v === "loaded") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Loaded.value);
        };
        if (v === "not-found") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(NotFound.value);
        };
        if (Data_Boolean.otherwise) {
            return Data_Argonaut_JCursor.fail(Data_Show.showString)("Not a LoadedState");
        };
        throw new Error("Failed pattern match at System.SystemD.Status line 22, column 5 - line 27, column 1: " + [ v.constructor.name ]);
    });
});
var decodeJsonActiveState = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeJsonString)(json))(function (v) {
        if (v === "active") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Active.value);
        };
        if (v === "failed") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Failed.value);
        };
        if (v === "inactive") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(Inactive.value);
        };
        if (Data_Boolean.otherwise) {
            return Data_Argonaut_JCursor.fail(Data_Show.showString)("Not a ActiveState");
        };
        throw new Error("Failed pattern match at System.SystemD.Status line 40, column 5 - line 47, column 1: " + [ v.constructor.name ]);
    });
});
var decodeJsonSystemDStatus = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeStrMap(Data_Argonaut_Decode_Class.decodeJsonJson))(json))(function (v) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeJsonString)(v)("name"))(function (v1) {
            return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeJsonString)(v)("description"))(function (v2) {
                return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(decodeJsonLoadedState)(v)("loadedState"))(function (v3) {
                    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeJsonString)(v)("loadedStateExtra"))(function (v4) {
                        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(decodeJsonActiveState)(v)("activeState"))(function (v5) {
                            return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeJsonMaybe(Data_Argonaut_Decode_Class.decodeJsonString))(v)("activeStateSince"))(function (v6) {
                                if (v6 instanceof Data_Maybe.Nothing) {
                                    return Control_Applicative.pure(Data_Either.applicativeEither)({
                                        name: v1,
                                        description: v2,
                                        loadedState: v3,
                                        loadedStateExtra: v4,
                                        activeState: v5,
                                        activeStateSince: Data_Maybe.Nothing.value
                                    });
                                };
                                if (v6 instanceof Data_Maybe.Just) {
                                    var v7 = Data_JSDate.toDateTime(Control_Monad_Eff_Unsafe.unsafePerformEff(Data_JSDate.parse(v6.value0)));
                                    if (v7 instanceof Data_Maybe.Nothing) {
                                        return Data_Argonaut_JCursor.fail(Data_Show.showString)("couldn't parse iso8601");
                                    };
                                    if (v7 instanceof Data_Maybe.Just) {
                                        return Control_Applicative.pure(Data_Either.applicativeEither)({
                                            name: v1,
                                            description: v2,
                                            loadedState: v3,
                                            loadedStateExtra: v4,
                                            activeState: v5,
                                            activeStateSince: new Data_Maybe.Just(v7.value0)
                                        });
                                    };
                                    throw new Error("Failed pattern match at System.SystemD.Status line 82, column 9 - line 86, column 98: " + [ v7.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at System.SystemD.Status line 78, column 5 - line 86, column 98: " + [ v6.constructor.name ]);
                            });
                        });
                    });
                });
            });
        });
    });
});
module.exports = {
    Loaded: Loaded,
    NotFound: NotFound,
    Active: Active,
    Failed: Failed,
    Inactive: Inactive,
    SystemDStatus: SystemDStatus,
    encodeJsonLoadedState: encodeJsonLoadedState,
    decodeJsonLoadedState: decodeJsonLoadedState,
    encodeJsonActiveState: encodeJsonActiveState,
    decodeJsonActiveState: decodeJsonActiveState,
    showSystemDStatus: showSystemDStatus,
    encodeJsonSystemDStatus: encodeJsonSystemDStatus,
    decodeJsonSystemDStatus: decodeJsonSystemDStatus
};

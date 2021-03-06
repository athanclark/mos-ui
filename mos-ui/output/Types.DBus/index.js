// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DBus_Signature = require("../DBus.Signature");
var Data_Argonaut = require("../Data.Argonaut");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class");
var Data_Argonaut_Decode_Combinators = require("../Data.Argonaut.Decode.Combinators");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Argonaut_Encode_Combinators = require("../Data.Argonaut.Encode.Combinators");
var Data_Argonaut_JCursor = require("../Data.Argonaut.JCursor");
var Data_Argonaut_Parser = require("../Data.Argonaut.Parser");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Monerodo_MoneroD = require("../Monerodo.MoneroD");
var Prelude = require("../Prelude");
var System_SystemD_Status = require("../System.SystemD.Status");
var Type_Proxy = require("../Type.Proxy");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var MoneroDLogSignal = (function () {
    function MoneroDLogSignal(value0) {
        this.value0 = value0;
    };
    MoneroDLogSignal.create = function (value0) {
        return new MoneroDLogSignal(value0);
    };
    return MoneroDLogSignal;
})();
var ServiceMoneroD = (function () {
    function ServiceMoneroD() {

    };
    ServiceMoneroD.value = new ServiceMoneroD();
    return ServiceMoneroD;
})();
var GotServiceState = (function () {
    function GotServiceState(value0) {
        this.value0 = value0;
    };
    GotServiceState.create = function (value0) {
        return new GotServiceState(value0);
    };
    return GotServiceState;
})();
var GetServiceState = (function () {
    function GetServiceState(value0) {
        this.value0 = value0;
    };
    GetServiceState.create = function (value0) {
        return new GetServiceState(value0);
    };
    return GetServiceState;
})();
var SignalOutput = (function () {
    function SignalOutput(value0) {
        this.value0 = value0;
    };
    SignalOutput.create = function (value0) {
        return new SignalOutput(value0);
    };
    return SignalOutput;
})();
var ControlOutput = (function () {
    function ControlOutput(value0) {
        this.value0 = value0;
    };
    ControlOutput.create = function (value0) {
        return new ControlOutput(value0);
    };
    return ControlOutput;
})();
var showAllInputs = new Data_Show.Show(Unsafe_Coerce.unsafeCoerce);
var encodeJsonSignalOutput = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    return Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Monerodo_MoneroD.encodeJsonMoneroDLog)("monerod")(v.value0))(Data_Argonaut_Core.jsonEmptyObject);
});
var encodeJsonService = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    return Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeJsonJString)("monerod");
});
var encodeJsonControlOutput = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    return Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonArray(System_SystemD_Status.encodeJsonSystemDStatus))("gotServiceState")(v.value0))(Data_Argonaut_Core.jsonEmptyObject);
});
var showControlOutput = new Data_Show.Show(function (x) {
    return Data_Show.show(Data_Argonaut_Core.showJson)(Data_Argonaut_Encode_Class.encodeJson(encodeJsonControlOutput)(x));
});
var encodeJsonControlInput = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    return Data_Argonaut_Encode_Combinators.extend(Data_Argonaut_Encode_Class.encodeJsonJson)(Data_Argonaut_Encode_Combinators.assoc(Data_Argonaut_Encode_Class.encodeJsonMaybe(encodeJsonService))("getServiceState")(v.value0))(Data_Argonaut_Core.jsonEmptyObject);
});
var encodeJsonAllInputs = new Data_Argonaut_Encode_Class.EncodeJson(function (v) {
    if (v instanceof SignalOutput) {
        return Data_Argonaut_Encode_Class.encodeJson(encodeJsonSignalOutput)(v.value0);
    };
    if (v instanceof ControlOutput) {
        return Data_Argonaut_Encode_Class.encodeJson(encodeJsonControlOutput)(v.value0);
    };
    throw new Error("Failed pattern match at Types.DBus line 22, column 1 - line 22, column 53: " + [ v.constructor.name ]);
});
var decodeJsonSignalOutput = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeStrMap(Data_Argonaut_Decode_Class.decodeJsonJson))(json))(function (v) {
        return Data_Functor.map(Data_Either.functorEither)(MoneroDLogSignal.create)(Data_Argonaut_Decode_Combinators.getField(Monerodo_MoneroD.decodeJsonMoneroDLog)(v)("monerod"));
    });
});
var isVariantSignalOutput = new DBus_Signature.IsVariant(function (v) {
    return Control_Bind.bind(Data_Either.bindEither)(DBus_Signature.fromVariant(DBus_Signature.isVariantString)(v))(function (v1) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Parser.jsonParser(v1))(Data_Argonaut_Decode_Class.decodeJson(decodeJsonSignalOutput));
    });
}, function ($36) {
    return DBus_Signature.toVariant(DBus_Signature.isVariantString)(Data_Show.show(Data_Argonaut_Core.showJson)(Data_Argonaut_Encode_Class.encodeJson(encodeJsonSignalOutput)($36)));
});
var isValueSignalOutput = new DBus_Signature.IsValue(function () {
    return isVariantSignalOutput;
}, function (v) {
    return DBus_Signature.typeOf(DBus_Signature.isValueString)(Type_Proxy["Proxy"].value);
});
var decodeJsonService = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeJsonString)(json))(function (v) {
        if (v === "monerod") {
            return Control_Applicative.pure(Data_Either.applicativeEither)(ServiceMoneroD.value);
        };
        if (Data_Boolean.otherwise) {
            return Data_Argonaut_JCursor.fail(Data_Show.showString)("Not a Service");
        };
        throw new Error("Failed pattern match at Types.DBus line 43, column 5 - line 48, column 1: " + [ v.constructor.name ]);
    });
});
var decodeJsonControlOutput = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeStrMap(Data_Argonaut_Decode_Class.decodeJsonJson))(json))(function (v) {
        var decodeServiceState = Data_Functor.map(Data_Either.functorEither)(GotServiceState.create)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeArray(System_SystemD_Status.decodeJsonSystemDStatus))(v)("gotServiceState"));
        return decodeServiceState;
    });
});
var isVariantControlOutput = new DBus_Signature.IsVariant(function (v) {
    return Control_Bind.bind(Data_Either.bindEither)(DBus_Signature.fromVariant(DBus_Signature.isVariantString)(v))(function (v1) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Parser.jsonParser(v1))(Data_Argonaut_Decode_Class.decodeJson(decodeJsonControlOutput));
    });
}, function ($37) {
    return DBus_Signature.toVariant(DBus_Signature.isVariantString)(Data_Show.show(Data_Argonaut_Core.showJson)(Data_Argonaut_Encode_Class.encodeJson(encodeJsonControlOutput)($37)));
});
var isValueControlOutput = new DBus_Signature.IsValue(function () {
    return isVariantControlOutput;
}, function (v) {
    return DBus_Signature.typeOf(DBus_Signature.isValueString)(Type_Proxy["Proxy"].value);
});
var decodeJsonControlInput = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeStrMap(Data_Argonaut_Decode_Class.decodeJsonJson))(json))(function (v) {
        var decodeServiceState = Data_Functor.map(Data_Either.functorEither)(GetServiceState.create)(Data_Argonaut_Decode_Combinators.getField(Data_Argonaut_Decode_Class.decodeJsonMaybe(decodeJsonService))(v)("getServiceState"));
        return decodeServiceState;
    });
});
var isVariantControlInput = new DBus_Signature.IsVariant(function (v) {
    return Control_Bind.bind(Data_Either.bindEither)(DBus_Signature.fromVariant(DBus_Signature.isVariantString)(v))(function (v1) {
        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Parser.jsonParser(v1))(Data_Argonaut_Decode_Class.decodeJson(decodeJsonControlInput));
    });
}, function ($38) {
    return DBus_Signature.toVariant(DBus_Signature.isVariantString)(Data_Show.show(Data_Argonaut_Core.showJson)(Data_Argonaut_Encode_Class.encodeJson(encodeJsonControlInput)($38)));
});
var isValueControlInput = new DBus_Signature.IsValue(function () {
    return isVariantControlInput;
}, function (v) {
    return DBus_Signature.typeOf(DBus_Signature.isValueString)(Type_Proxy["Proxy"].value);
});
var decodeJsonAllInputs = new Data_Argonaut_Decode_Class.DecodeJson(function (json) {
    return Control_Alt.alt(Data_Either.altEither)(Data_Functor.map(Data_Either.functorEither)(SignalOutput.create)(Data_Argonaut_Decode_Class.decodeJson(decodeJsonSignalOutput)(json)))(Data_Functor.map(Data_Either.functorEither)(ControlOutput.create)(Data_Argonaut_Decode_Class.decodeJson(decodeJsonControlOutput)(json)));
});
module.exports = {
    SignalOutput: SignalOutput,
    ControlOutput: ControlOutput,
    ServiceMoneroD: ServiceMoneroD,
    GetServiceState: GetServiceState,
    GotServiceState: GotServiceState,
    MoneroDLogSignal: MoneroDLogSignal,
    encodeJsonAllInputs: encodeJsonAllInputs,
    decodeJsonAllInputs: decodeJsonAllInputs,
    showAllInputs: showAllInputs,
    encodeJsonService: encodeJsonService,
    decodeJsonService: decodeJsonService,
    encodeJsonControlInput: encodeJsonControlInput,
    decodeJsonControlInput: decodeJsonControlInput,
    isVariantControlInput: isVariantControlInput,
    isValueControlInput: isValueControlInput,
    encodeJsonControlOutput: encodeJsonControlOutput,
    decodeJsonControlOutput: decodeJsonControlOutput,
    showControlOutput: showControlOutput,
    isVariantControlOutput: isVariantControlOutput,
    isValueControlOutput: isValueControlOutput,
    encodeJsonSignalOutput: encodeJsonSignalOutput,
    decodeJsonSignalOutput: decodeJsonSignalOutput,
    isVariantSignalOutput: isVariantSignalOutput,
    isValueSignalOutput: isValueSignalOutput
};

// Generated by purs version 0.11.6
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Lens = require("../Data.Lens.Lens");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Symbol = require("../Data.Symbol");
var Data_Tuple = require("../Data.Tuple");
var Data_URI_Host = require("../Data.URI.Host");
var Data_URI_Port = require("../Data.URI.Port");
var Data_URI_UserInfo = require("../Data.URI.UserInfo");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var Authority = (function () {
    function Authority(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Authority.create = function (value0) {
        return function (value1) {
            return new Authority(value0, value1);
        };
    };
    return Authority;
})();
var print = function (v) {
    var printUserInfo = Data_Maybe.maybe("")(function (u) {
        return Data_URI_UserInfo.print(u) + "@";
    })(v.value0);
    var printHostAndPort = function (v1) {
        return Data_URI_Host.print(v1.value0) + Data_Maybe.maybe("")(function (n) {
            return ":" + Data_URI_Port.print(n);
        })(v1.value1);
    };
    return "//" + (printUserInfo + Data_String.joinWith(",")(Data_Functor.map(Data_Functor.functorArray)(printHostAndPort)(v.value1)));
};
var parser = Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_String.string("//"))(function (v) {
    return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.optionMaybe(Text_Parsing_StringParser["try"](Control_Apply.applyFirst(Text_Parsing_StringParser.applyParser)(Data_URI_UserInfo.parser)(Text_Parsing_StringParser_String.string("@")))))(function (v1) {
        return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Data_Function.flip(Text_Parsing_StringParser_Combinators.sepBy)(Text_Parsing_StringParser_String.string(","))(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Tuple.Tuple.create)(Data_URI_Host.parser))(Text_Parsing_StringParser_Combinators.optionMaybe(Control_Apply.applySecond(Text_Parsing_StringParser.applyParser)(Text_Parsing_StringParser_String.string(":"))(Data_URI_Port.parser)))))(function (v2) {
            return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(new Authority(v1, Data_Array.fromFoldable(Data_List_Types.foldableList)(v2)));
        });
    });
});
var genericAuthority = new Data_Generic_Rep.Generic(function (x) {
    return new Data_Generic_Rep.Product(x.value0, x.value1);
}, function (x) {
    return new Authority(x.value0, x.value1);
});
var showAuthority = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericAuthority)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_UserInfo.showUserInfo)))(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Show.showArray(Data_Tuple.showTuple(Data_URI_Host.showHost)(Data_Maybe.showMaybe(Data_URI_Port.showPort))))))(new Data_Symbol.IsSymbol(function () {
    return "Authority";
}))));
var eqAuthority = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_UserInfo.eqUserInfo))(x.value0)(y.value0) && Data_Eq.eq(Data_Eq.eqArray(Data_Tuple.eqTuple(Data_URI_Host.eqHost)(Data_Maybe.eqMaybe(Data_URI_Port.eqPort))))(x.value1)(y.value1);
    };
});
var ordAuthority = new Data_Ord.Ord(function () {
    return eqAuthority;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_UserInfo.ordUserInfo))(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Ord.ordArray(Data_Tuple.ordTuple(Data_URI_Host.ordHost)(Data_Maybe.ordMaybe(Data_URI_Port.ordPort))))(x.value1)(y.value1);
    };
});
var _userInfo = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value0;
    })(function (v) {
        return function (ui) {
            return new Authority(ui, v.value1);
        };
    })(dictStrong);
};
var _hosts = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value1;
    })(function (v) {
        return function (hs) {
            return new Authority(v.value0, hs);
        };
    })(dictStrong);
};
module.exports = {
    Authority: Authority, 
    _hosts: _hosts, 
    _userInfo: _userInfo, 
    parser: parser, 
    print: print, 
    eqAuthority: eqAuthority, 
    ordAuthority: ordAuthority, 
    genericAuthority: genericAuthority, 
    showAuthority: showAuthority
};

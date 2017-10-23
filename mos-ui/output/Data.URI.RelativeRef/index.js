// Generated by purs version 0.11.6
"use strict";
var Control_Apply = require("../Control.Apply");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Lens = require("../Data.Lens.Lens");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Symbol = require("../Data.Symbol");
var Data_URI_Fragment = require("../Data.URI.Fragment");
var Data_URI_Query = require("../Data.URI.Query");
var Data_URI_RelativePart = require("../Data.URI.RelativePart");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var RelativeRef = (function () {
    function RelativeRef(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    RelativeRef.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new RelativeRef(value0, value1, value2);
            };
        };
    };
    return RelativeRef;
})();
var print = function (v) {
    return Data_String.joinWith("")(Data_Array.catMaybes([ new Data_Maybe.Just(Data_URI_RelativePart.print(v.value0)), Data_Functor.map(Data_Maybe.functorMaybe)(Data_URI_Query.print)(v.value1), Data_Functor.map(Data_Maybe.functorMaybe)(Data_URI_Fragment.print)(v.value2) ]));
};
var parser = Control_Apply.applyFirst(Text_Parsing_StringParser.applyParser)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(RelativeRef.create)(Data_URI_RelativePart.parser))(Text_Parsing_StringParser_Combinators.optionMaybe(Data_URI_Query.parser)))(Text_Parsing_StringParser_Combinators.optionMaybe(Data_URI_Fragment.parser)))(Text_Parsing_StringParser_String.eof);
var parse = Text_Parsing_StringParser.runParser(parser);
var genericRelativeRef = new Data_Generic_Rep.Generic(function (x) {
    return new Data_Generic_Rep.Product(x.value0, new Data_Generic_Rep.Product(x.value1, x.value2));
}, function (x) {
    return new RelativeRef(x.value0, x.value1.value0, x.value1.value1);
});
var showRelativeRef = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericRelativeRef)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_URI_RelativePart.showRelativePart))(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_Query.showQuery)))(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_Fragment.showFragment)))))(new Data_Symbol.IsSymbol(function () {
    return "RelativeRef";
}))));
var eqRelativeRef = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_URI_RelativePart.eqRelativePart)(x.value0)(y.value0) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_Query.eqQuery))(x.value1)(y.value1) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_Fragment.eqFragment))(x.value2)(y.value2);
    };
});
var ordRelativeRef = new Data_Ord.Ord(function () {
    return eqRelativeRef;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_URI_RelativePart.ordRelativePart)(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v1 = Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_Query.ordQuery))(x.value1)(y.value1);
        if (v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_Fragment.ordFragment))(x.value2)(y.value2);
    };
});
var _relPart = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value0;
    })(function (v) {
        return function (r) {
            return new RelativeRef(r, v.value1, v.value2);
        };
    })(dictStrong);
};
var _query = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value1;
    })(function (v) {
        return function (q) {
            return new RelativeRef(v.value0, q, v.value2);
        };
    })(dictStrong);
};
var _fragment = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value2;
    })(function (v) {
        return function (f) {
            return new RelativeRef(v.value0, v.value1, f);
        };
    })(dictStrong);
};
module.exports = {
    RelativeRef: RelativeRef, 
    _fragment: _fragment, 
    _query: _query, 
    _relPart: _relPart, 
    parse: parse, 
    parser: parser, 
    print: print, 
    eqRelativeRef: eqRelativeRef, 
    ordRelativeRef: ordRelativeRef, 
    genericRelativeRef: genericRelativeRef, 
    showRelativeRef: showRelativeRef
};
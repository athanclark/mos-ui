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
var Data_URI_HierarchicalPart = require("../Data.URI.HierarchicalPart");
var Data_URI_Query = require("../Data.URI.Query");
var Data_URI_Scheme = require("../Data.URI.Scheme");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var URI = (function () {
    function URI(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    URI.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new URI(value0, value1, value2, value3);
                };
            };
        };
    };
    return URI;
})();
var print = function (v) {
    return Data_String.joinWith("")(Data_Array.catMaybes([ Data_Functor.map(Data_Maybe.functorMaybe)(Data_URI_Scheme.print)(v.value0), new Data_Maybe.Just(Data_URI_HierarchicalPart.print(v.value1)), Data_Functor.map(Data_Maybe.functorMaybe)(Data_URI_Query.print)(v.value2), Data_Functor.map(Data_Maybe.functorMaybe)(Data_URI_Fragment.print)(v.value3) ]));
};
var parser = Control_Apply.applyFirst(Text_Parsing_StringParser.applyParser)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(URI.create)(Text_Parsing_StringParser_Combinators.optionMaybe(Data_URI_Scheme.parser)))(Data_URI_HierarchicalPart.parser))(Text_Parsing_StringParser_Combinators.optionMaybe(Data_URI_Query.parser)))(Text_Parsing_StringParser_Combinators.optionMaybe(Data_URI_Fragment.parser)))(Text_Parsing_StringParser_String.eof);
var parse = Text_Parsing_StringParser.runParser(parser);
var genericURI = new Data_Generic_Rep.Generic(function (x) {
    return new Data_Generic_Rep.Product(x.value0, new Data_Generic_Rep.Product(x.value1, new Data_Generic_Rep.Product(x.value2, x.value3)));
}, function (x) {
    return new URI(x.value0, x.value1.value0, x.value1.value1.value0, x.value1.value1.value1);
});
var showURI = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericURI)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_Scheme.showScheme)))(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_URI_HierarchicalPart.showHierarchicalPart))(Data_Generic_Rep_Show.genericShowArgsProduct(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_Query.showQuery)))(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Maybe.showMaybe(Data_URI_Fragment.showFragment))))))(new Data_Symbol.IsSymbol(function () {
    return "URI";
}))));
var eqURI = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_Scheme.eqScheme))(x.value0)(y.value0) && Data_Eq.eq(Data_URI_HierarchicalPart.eqHierarchicalPart)(x.value1)(y.value1) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_Query.eqQuery))(x.value2)(y.value2) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_URI_Fragment.eqFragment))(x.value3)(y.value3);
    };
});
var ordURI = new Data_Ord.Ord(function () {
    return eqURI;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_Scheme.ordScheme))(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v1 = Data_Ord.compare(Data_URI_HierarchicalPart.ordHierarchicalPart)(x.value1)(y.value1);
        if (v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v2 = Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_Query.ordQuery))(x.value2)(y.value2);
        if (v2 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v2 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Maybe.ordMaybe(Data_URI_Fragment.ordFragment))(x.value3)(y.value3);
    };
});
var _scheme = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value0;
    })(function (v) {
        return function (s) {
            return new URI(s, v.value1, v.value2, v.value3);
        };
    })(dictStrong);
};
var _query = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value2;
    })(function (v) {
        return function (q) {
            return new URI(v.value0, v.value1, q, v.value3);
        };
    })(dictStrong);
};
var _hierPart = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value1;
    })(function (v) {
        return function (h) {
            return new URI(v.value0, h, v.value2, v.value3);
        };
    })(dictStrong);
};
var _fragment = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value3;
    })(function (v) {
        return function (f) {
            return new URI(v.value0, v.value1, v.value2, f);
        };
    })(dictStrong);
};
module.exports = {
    URI: URI, 
    _fragment: _fragment, 
    _hierPart: _hierPart, 
    _query: _query, 
    _scheme: _scheme, 
    parse: parse, 
    parser: parser, 
    print: print, 
    eqURI: eqURI, 
    ordURI: ordURI, 
    genericURI: genericURI, 
    showURI: showURI
};

// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex = require("../Data.String.Regex");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags");
var Data_Symbol = require("../Data.Symbol");
var Data_Tuple = require("../Data.Tuple");
var Data_URI_Common = require("../Data.URI.Common");
var Global = require("../Global");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var Query = function (x) {
    return x;
};
var semigroupQuery = Data_List_Types.semigroupList;
var rxPrintable = Data_Either.fromRight()(Data_String_Regex.regex("[$+/?:@]")(Data_String_Regex_Flags.global));
var printQueryPart = (function () {
    var printChar = function (s) {
        if (Data_String_Regex.test(rxPrintable)(s)) {
            return s;
        };
        if (Data_Boolean.otherwise) {
            return Global["encodeURIComponent"](s);
        };
        throw new Error("Failed pattern match at Data.URI.Query line 67, column 3 - line 67, column 30: " + [ s.constructor.name ]);
    };
    return function ($23) {
        return Data_String.joinWith("")(Data_Functor.map(Data_Functor.functorArray)(printChar)(Data_String.split("")($23)));
    };
})();
var print = function (v) {
    var printPart = function (v1) {
        if (v1.value1 instanceof Data_Maybe.Nothing) {
            return printQueryPart(v1.value0);
        };
        if (v1.value1 instanceof Data_Maybe.Just) {
            return printQueryPart(v1.value0) + ("=" + printQueryPart(v1.value1.value0));
        };
        throw new Error("Failed pattern match at Data.URI.Query line 57, column 3 - line 57, column 51: " + [ v1.constructor.name ]);
    };
    if (v instanceof Data_List_Types.Nil) {
        return "?";
    };
    return "?" + Data_URI_Common.joinWith("&")(Data_Functor.map(Data_List_Types.functorList)(printPart)(v));
};
var parsePart = Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Global["decodeURIComponent"])(Data_URI_Common.rxPat("[^=;&]+")))(function (v) {
    return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_Combinators.optionMaybe(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Global["decodeURIComponent"])(Control_Apply.applySecond(Text_Parsing_StringParser.applyParser)(Text_Parsing_StringParser_String.string("="))(Data_URI_Common.rxPat("[^;&]*")))))(function (v1) {
        return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(new Data_Tuple.Tuple(v, v1));
    });
});
var parseParts = Text_Parsing_StringParser_Combinators.sepBy(parsePart)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Text_Parsing_StringParser_String.string(";"))(Text_Parsing_StringParser_String.string("&")));
var parser = Control_Apply.applySecond(Text_Parsing_StringParser.applyParser)(Text_Parsing_StringParser_String.string("?"))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Query)(Data_URI_Common.wrapParser(parseParts)(Text_Parsing_StringParser["try"](Data_URI_Common.rxPat("[^#]*")))));
var ordQuery = Data_List_Types.ordList(Data_Tuple.ordTuple(Data_Ord.ordString)(Data_Maybe.ordMaybe(Data_Ord.ordString)));
var newtypeQuery = new Data_Newtype.Newtype(function (n) {
    return n;
}, Query);
var monoidQuery = Data_List_Types.monoidList;
var genericQuery = new Data_Generic_Rep.Generic(function (x) {
    return x;
}, function (x) {
    return x;
});
var showQuery = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericQuery)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsArgument(Data_List_Types.showList(Data_Tuple.showTuple(Data_Show.showString)(Data_Maybe.showMaybe(Data_Show.showString)))))(new Data_Symbol.IsSymbol(function () {
    return "Query";
}))));
var eqQuery = Data_List_Types.eqList(Data_Tuple.eqTuple(Data_Eq.eqString)(Data_Maybe.eqMaybe(Data_Eq.eqString)));
module.exports = {
    Query: Query,
    parser: parser,
    print: print,
    eqQuery: eqQuery,
    ordQuery: ordQuery,
    genericQuery: genericQuery,
    newtypeQuery: newtypeQuery,
    showQuery: showQuery,
    semigroupQuery: semigroupQuery,
    monoidQuery: monoidQuery
};

// Generated by purs version 0.11.6
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex = require("../Data.String.Regex");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags");
var Data_Symbol = require("../Data.Symbol");
var Data_URI_Common = require("../Data.URI.Common");
var Global = require("../Global");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var Fragment = function (x) {
    return x;
};
var rxPrintable = Data_Either.fromRight()(Data_String_Regex.regex("[&;$+=/?:@]")(Data_String_Regex_Flags.global));
var print = function (v) {
    var printChar = function (s) {
        if (Data_String_Regex.test(rxPrintable)(s)) {
            return s;
        };
        if (Data_Boolean.otherwise) {
            return Global["encodeURIComponent"](s);
        };
        throw new Error("Failed pattern match at Data.URI.Fragment line 39, column 3 - line 39, column 30: " + [ s.constructor.name ]);
    };
    return "#" + Data_String.joinWith("")(Data_Functor.map(Data_Functor.functorArray)(printChar)(Data_String.split("")(v)));
};
var parser = Control_Apply.applySecond(Text_Parsing_StringParser.applyParser)(Text_Parsing_StringParser_String.string("#"))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(function ($11) {
    return Fragment(Data_URI_Common.joinWith("")($11));
})(Text_Parsing_StringParser_Combinators.many(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Data_URI_Common.parsePChar(Data_URI_Common.decodePCTComponent))(Text_Parsing_StringParser_String.string("/")))(Text_Parsing_StringParser_String.string("?")))));
var ordFragment = Data_Ord.ordString;
var newtypeFragment = new Data_Newtype.Newtype(function (n) {
    return n;
}, Fragment);
var genericFragment = new Data_Generic_Rep.Generic(function (x) {
    return x;
}, function (x) {
    return x;
});
var showFragment = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericFragment)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Show.showString))(new Data_Symbol.IsSymbol(function () {
    return "Fragment";
}))));
var eqFragment = Data_Eq.eqString;
module.exports = {
    Fragment: Fragment, 
    parser: parser, 
    print: print, 
    rxPrintable: rxPrintable, 
    eqFragment: eqFragment, 
    ordFragment: ordFragment, 
    genericFragment: genericFragment, 
    newtypeFragment: newtypeFragment, 
    showFragment: showFragment
};

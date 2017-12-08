// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Show = require("../Data.Show");
var Data_Symbol = require("../Data.Symbol");
var Data_URI_Common = require("../Data.URI.Common");
var Global = require("../Global");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var UserInfo = function (x) {
    return x;
};
var print = function (v) {
    return Global["encodeURI"](v);
};
var parser = (function () {
    var p = Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Data_URI_Common.parseUnreserved)(Data_URI_Common.parsePCTEncoded(Data_URI_Common.decodePCT)))(Data_URI_Common.parseSubDelims))(Text_Parsing_StringParser_String.string(":"));
    return Data_Functor.map(Text_Parsing_StringParser.functorParser)(function ($9) {
        return UserInfo(Data_URI_Common.joinWith("")($9));
    })(Text_Parsing_StringParser_Combinators.many1(p));
})();
var ordUserInfo = Data_Ord.ordString;
var newtypeUserInfo = new Data_Newtype.Newtype(function (n) {
    return n;
}, UserInfo);
var genericUserInfo = new Data_Generic_Rep.Generic(function (x) {
    return x;
}, function (x) {
    return x;
});
var showUserInfo = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericUserInfo)(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsArgument(Data_Show.showString))(new Data_Symbol.IsSymbol(function () {
    return "UserInfo";
}))));
var eqUserInfo = Data_Eq.eqString;
module.exports = {
    UserInfo: UserInfo,
    parser: parser,
    print: print,
    eqUserInfo: eqUserInfo,
    ordUserInfo: ordUserInfo,
    genericUserInfo: genericUserInfo,
    newtypeUserInfo: newtypeUserInfo,
    showUserInfo: showUserInfo
};

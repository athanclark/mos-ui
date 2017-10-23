// Generated by purs version 0.11.6
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Path_Pathy = require("../Data.Path.Pathy");
var Data_Semigroup = require("../Data.Semigroup");
var Data_String = require("../Data.String");
var Data_URI_Common = require("../Data.URI.Common");
var Global = require("../Global");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var Text_Parsing_StringParser_String = require("../Text.Parsing.StringParser.String");
var parseURIPathRel = function (v) {
    var v1 = Data_Path_Pathy.parseRelFile(Data_String.drop(v.pos)(v.str));
    if (v1 instanceof Data_Maybe.Just) {
        return new Data_Either.Right({
            result: new Data_Either.Right(v1.value0), 
            suffix: {
                str: v.str, 
                pos: Data_String.length(v.str)
            }
        });
    };
    if (v1 instanceof Data_Maybe.Nothing) {
        var v2 = Data_Path_Pathy.parseRelDir(Data_String.drop(v.pos)(v.str));
        if (v2 instanceof Data_Maybe.Just) {
            return new Data_Either.Right({
                result: new Data_Either.Left(v2.value0), 
                suffix: {
                    str: v.str, 
                    pos: Data_String.length(v.str)
                }
            });
        };
        if (v2 instanceof Data_Maybe.Nothing) {
            return new Data_Either.Left({
                error: Text_Parsing_StringParser.ParseError.create("Expected a valid path"), 
                pos: v.pos
            });
        };
        throw new Error("Failed pattern match at Data.URI.Path line 97, column 15 - line 99, column 78: " + [ v2.constructor.name ]);
    };
    throw new Error("Failed pattern match at Data.URI.Path line 95, column 3 - line 99, column 78: " + [ v1.constructor.name ]);
};
var parseURIPathAbs = function (v) {
    var v1 = Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Path_Pathy.sandbox(Data_Path_Pathy.rootDir))(Data_Path_Pathy.parseAbsFile(Data_String.drop(v.pos)(v.str)));
    if (v1 instanceof Data_Maybe.Just) {
        return new Data_Either.Right({
            result: Data_Either.Right.create(Data_Path_Pathy.appendPath(Data_Path_Pathy.rootDir)(v1.value0)), 
            suffix: {
                str: v.str, 
                pos: Data_String.length(v.str)
            }
        });
    };
    if (v1 instanceof Data_Maybe.Nothing) {
        var v2 = Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Path_Pathy.sandbox(Data_Path_Pathy.rootDir))(Data_Path_Pathy.parseAbsDir(Data_String.drop(v.pos)(v.str)));
        if (v2 instanceof Data_Maybe.Just) {
            return new Data_Either.Right({
                result: Data_Either.Left.create(Data_Path_Pathy.appendPath(Data_Path_Pathy.rootDir)(v2.value0)), 
                suffix: {
                    str: v.str, 
                    pos: Data_String.length(v.str)
                }
            });
        };
        if (v2 instanceof Data_Maybe.Nothing) {
            return new Data_Either.Left({
                error: Text_Parsing_StringParser.ParseError.create("Expected a valid path"), 
                pos: v.pos
            });
        };
        throw new Error("Failed pattern match at Data.URI.Path line 89, column 15 - line 91, column 79: " + [ v2.constructor.name ]);
    };
    throw new Error("Failed pattern match at Data.URI.Path line 87, column 3 - line 91, column 79: " + [ v1.constructor.name ]);
};
var escaper = Data_Path_Pathy.Escaper(function ($25) {
    return Data_String.replaceAll("#")("%23")(Global["encodeURI"]($25));
});
var printPath$prime = function (path) {
    var printed = Data_Path_Pathy["unsafePrintPath'"](escaper)(path);
    return Data_Maybe.fromMaybe(printed)(Data_String.stripPrefix("./")(printed));
};
var printPath = Data_Either.either(printPath$prime)(printPath$prime);
var decoder = function ($26) {
    return Data_String.replaceAll("%23")("#")(Data_URI_Common.decodePCT($26));
};
var parseSegment = Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many(Data_URI_Common.parsePChar(decoder)));
var parsePathAbEmpty = function (p) {
    return Control_Alt.alt(Text_Parsing_StringParser.altParser)(Text_Parsing_StringParser["try"](Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Maybe.Just.create)(Data_URI_Common.wrapParser(p)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(Text_Parsing_StringParser_String.string("/")))(parseSegment)))))))(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(Data_Maybe.Nothing.value));
};
var parseSegmentNonZero = Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many1(Data_URI_Common.parsePChar(decoder)));
var parsePathAbsolute = function (p) {
    return Data_URI_Common.wrapParser(p)(Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Text_Parsing_StringParser_String.string("/"))(function (v) {
        return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(parseSegmentNonZero)(function (v1) {
            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(Text_Parsing_StringParser_String.string("/")))(parseSegment))))(function (v2) {
                return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)("/" + (v1 + v2));
            });
        });
    }));
};
var parsePathRootless = function (p) {
    return Data_URI_Common.wrapParser(p)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(parseSegmentNonZero))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(Text_Parsing_StringParser_String.string("/")))(parseSegment)))));
};
var parseSegmentNonZeroNoColon = Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many1(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Data_URI_Common.parseUnreserved)(Data_URI_Common.parsePCTEncoded(decoder)))(Data_URI_Common.parseSubDelims))(Text_Parsing_StringParser_String.string("@"))));
var parsePathNoScheme = function (p) {
    return Data_URI_Common.wrapParser(p)(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(parseSegmentNonZeroNoColon))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_URI_Common.joinWith(""))(Text_Parsing_StringParser_Combinators.many(Control_Apply.apply(Text_Parsing_StringParser.applyParser)(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Semigroup.append(Data_Semigroup.semigroupString))(Text_Parsing_StringParser_String.string("/")))(parseSegment)))));
};
var parsePath = function (p) {
    return Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(parsePathAbEmpty(p))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Maybe.Just.create)(parsePathAbsolute(p))))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Maybe.Just.create)(parsePathNoScheme(p))))(Data_Functor.map(Text_Parsing_StringParser.functorParser)(Data_Maybe.Just.create)(parsePathRootless(p))))(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(Data_Maybe.Nothing.value));
};
module.exports = {
    parsePath: parsePath, 
    parsePathAbEmpty: parsePathAbEmpty, 
    parsePathAbsolute: parsePathAbsolute, 
    parsePathNoScheme: parsePathNoScheme, 
    parsePathRootless: parsePathRootless, 
    parseSegment: parseSegment, 
    parseSegmentNonZero: parseSegmentNonZero, 
    parseSegmentNonZeroNoColon: parseSegmentNonZeroNoColon, 
    parseURIPathAbs: parseURIPathAbs, 
    parseURIPathRel: parseURIPathRel, 
    printPath: printPath
};
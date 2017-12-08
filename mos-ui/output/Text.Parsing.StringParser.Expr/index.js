// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Data_Foldable = require("../Data.Foldable");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Prelude = require("../Prelude");
var Text_Parsing_StringParser = require("../Text.Parsing.StringParser");
var Text_Parsing_StringParser_Combinators = require("../Text.Parsing.StringParser.Combinators");
var AssocNone = (function () {
    function AssocNone() {

    };
    AssocNone.value = new AssocNone();
    return AssocNone;
})();
var AssocLeft = (function () {
    function AssocLeft() {

    };
    AssocLeft.value = new AssocLeft();
    return AssocLeft;
})();
var AssocRight = (function () {
    function AssocRight() {

    };
    AssocRight.value = new AssocRight();
    return AssocRight;
})();
var Infix = (function () {
    function Infix(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Infix.create = function (value0) {
        return function (value1) {
            return new Infix(value0, value1);
        };
    };
    return Infix;
})();
var Prefix = (function () {
    function Prefix(value0) {
        this.value0 = value0;
    };
    Prefix.create = function (value0) {
        return new Prefix(value0);
    };
    return Prefix;
})();
var Postfix = (function () {
    function Postfix(value0) {
        this.value0 = value0;
    };
    Postfix.create = function (value0) {
        return new Postfix(value0);
    };
    return Postfix;
})();
var buildExprParser = function (operators) {
    return function (simpleExpr) {
        var termP = function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(prefixP)(function (v) {
                        return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(term)(function (v1) {
                            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(postfixP)(function (v2) {
                                return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v2(v(v1)));
                            });
                        });
                    });
                };
            };
        };
        var splitOp = function (v) {
            return function (accum) {
                if (v instanceof Infix && v.value1 instanceof AssocNone) {
                    var $22 = {};
                    for (var $23 in accum) {
                        if ({}.hasOwnProperty.call(accum, $23)) {
                            $22[$23] = accum[$23];
                        };
                    };
                    $22.nassoc = new Data_List_Types.Cons(v.value0, accum.nassoc);
                    return $22;
                };
                if (v instanceof Infix && v.value1 instanceof AssocLeft) {
                    var $27 = {};
                    for (var $28 in accum) {
                        if ({}.hasOwnProperty.call(accum, $28)) {
                            $27[$28] = accum[$28];
                        };
                    };
                    $27.lassoc = new Data_List_Types.Cons(v.value0, accum.lassoc);
                    return $27;
                };
                if (v instanceof Infix && v.value1 instanceof AssocRight) {
                    var $32 = {};
                    for (var $33 in accum) {
                        if ({}.hasOwnProperty.call(accum, $33)) {
                            $32[$33] = accum[$33];
                        };
                    };
                    $32.rassoc = new Data_List_Types.Cons(v.value0, accum.rassoc);
                    return $32;
                };
                if (v instanceof Prefix) {
                    var $37 = {};
                    for (var $38 in accum) {
                        if ({}.hasOwnProperty.call(accum, $38)) {
                            $37[$38] = accum[$38];
                        };
                    };
                    $37.prefix = new Data_List_Types.Cons(v.value0, accum.prefix);
                    return $37;
                };
                if (v instanceof Postfix) {
                    var $41 = {};
                    for (var $42 in accum) {
                        if ({}.hasOwnProperty.call(accum, $42)) {
                            $41[$42] = accum[$42];
                        };
                    };
                    $41.postfix = new Data_List_Types.Cons(v.value0, accum.postfix);
                    return $41;
                };
                throw new Error("Failed pattern match at Text.Parsing.StringParser.Expr line 59, column 5 - line 59, column 68: " + [ v.constructor.name, accum.constructor.name ]);
            };
        };
        var rassocP1 = function (x) {
            return function (rassocOp) {
                return function (prefixP) {
                    return function (term) {
                        return function (postfixP) {
                            return Control_Alt.alt(Text_Parsing_StringParser.altParser)(rassocP(x)(rassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(x));
                        };
                    };
                };
            };
        };
        var rassocP = function (x) {
            return function (rassocOp) {
                return function (prefixP) {
                    return function (term) {
                        return function (postfixP) {
                            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(rassocOp)(function (v) {
                                return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(Control_Bind.bind(Text_Parsing_StringParser.bindParser)(termP(prefixP)(term)(postfixP))(function (v1) {
                                    return rassocP1(v1)(rassocOp)(prefixP)(term)(postfixP);
                                }))(function (v1) {
                                    return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v(x)(v1));
                                });
                            });
                        };
                    };
                };
            };
        };
        var nassocP = function (x) {
            return function (nassocOp) {
                return function (prefixP) {
                    return function (term) {
                        return function (postfixP) {
                            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(nassocOp)(function (v) {
                                return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(termP(prefixP)(term)(postfixP))(function (v1) {
                                    return Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v(x)(v1));
                                });
                            });
                        };
                    };
                };
            };
        };
        var lassocP1 = function (x) {
            return function (lassocOp) {
                return function (prefixP) {
                    return function (term) {
                        return function (postfixP) {
                            return Control_Alt.alt(Text_Parsing_StringParser.altParser)(lassocP(x)(lassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(x));
                        };
                    };
                };
            };
        };
        var lassocP = function (x) {
            return function (lassocOp) {
                return function (prefixP) {
                    return function (term) {
                        return function (postfixP) {
                            return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(lassocOp)(function (v) {
                                return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(termP(prefixP)(term)(postfixP))(function (v1) {
                                    return lassocP1(v(x)(v1))(lassocOp)(prefixP)(term)(postfixP);
                                });
                            });
                        };
                    };
                };
            };
        };
        var makeParser = function (term) {
            return function (ops) {
                var accum = Data_Foldable.foldr(Data_Foldable.foldableArray)(splitOp)({
                    rassoc: Data_List_Types.Nil.value,
                    lassoc: Data_List_Types.Nil.value,
                    nassoc: Data_List_Types.Nil.value,
                    prefix: Data_List_Types.Nil.value,
                    postfix: Data_List_Types.Nil.value
                })(ops);
                var lassocOp = Text_Parsing_StringParser_Combinators.choice(Data_List_Types.foldableList)(accum.lassoc);
                var nassocOp = Text_Parsing_StringParser_Combinators.choice(Data_List_Types.foldableList)(accum.nassoc);
                var postfixOp = Text_Parsing_StringParser_Combinators.withError(Text_Parsing_StringParser_Combinators.choice(Data_List_Types.foldableList)(accum.postfix))("");
                var postfixP = Control_Alt.alt(Text_Parsing_StringParser.altParser)(postfixOp)(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(Control_Category.id(Control_Category.categoryFn)));
                var prefixOp = Text_Parsing_StringParser_Combinators.withError(Text_Parsing_StringParser_Combinators.choice(Data_List_Types.foldableList)(accum.prefix))("");
                var prefixP = Control_Alt.alt(Text_Parsing_StringParser.altParser)(prefixOp)(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(Control_Category.id(Control_Category.categoryFn)));
                var rassocOp = Text_Parsing_StringParser_Combinators.choice(Data_List_Types.foldableList)(accum.rassoc);
                return Control_Bind.bind(Text_Parsing_StringParser.bindParser)(termP(prefixP)(term)(postfixP))(function (v) {
                    return Text_Parsing_StringParser_Combinators.withError(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(Control_Alt.alt(Text_Parsing_StringParser.altParser)(rassocP(v)(rassocOp)(prefixP)(term)(postfixP))(lassocP(v)(lassocOp)(prefixP)(term)(postfixP)))(nassocP(v)(nassocOp)(prefixP)(term)(postfixP)))(Control_Applicative.pure(Text_Parsing_StringParser.applicativeParser)(v)))("operator");
                });
            };
        };
        return Data_Foldable.foldl(Data_Foldable.foldableArray)(makeParser)(simpleExpr)(operators);
    };
};
module.exports = {
    AssocNone: AssocNone,
    AssocLeft: AssocLeft,
    AssocRight: AssocRight,
    Infix: Infix,
    Prefix: Prefix,
    Postfix: Postfix,
    buildExprParser: buildExprParser
};

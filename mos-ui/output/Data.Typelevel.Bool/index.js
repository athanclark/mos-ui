// Generated by purs version 0.11.6
"use strict";
var Data_Show = require("../Data.Show");
var Data_Typelevel_Undefined = require("../Data.Typelevel.Undefined");
var BoolI = function (toBool) {
    this.toBool = toBool;
};
var Bool = function (BoolI0) {
    this.BoolI0 = BoolI0;
};
var Not = function (BoolI0, BoolI1) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
};
var And = function (BoolI0, BoolI1, BoolI2) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
    this.BoolI2 = BoolI2;
};
var Or = function (BoolI0, BoolI1, BoolI2) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
    this.BoolI2 = BoolI2;
};
var Xor = function (BoolI0, BoolI1, BoolI2) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
    this.BoolI2 = BoolI2;
};
var Imp = function (BoolI0, BoolI1, BoolI2) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
    this.BoolI2 = BoolI2;
};
var Eq = function (BoolI0, BoolI1, BoolI2) {
    this.BoolI0 = BoolI0;
    this.BoolI1 = BoolI1;
    this.BoolI2 = BoolI2;
};
var xor = function (dictXor) {
    return Data_Typelevel_Undefined["undefined"];
};
var trueT = Data_Typelevel_Undefined["undefined"];
var toBool = function (dict) {
    return dict.toBool;
};
var showTrue = new Data_Show.Show(function (v) {
    return "True";
});
var showFalse = new Data_Show.Show(function (v) {
    return "False";
});
var or = function (dictOr) {
    return Data_Typelevel_Undefined["undefined"];
};
var not = function (dictNot) {
    return Data_Typelevel_Undefined["undefined"];
};
var imp = function (dictImp) {
    return Data_Typelevel_Undefined["undefined"];
};
var falseT = Data_Typelevel_Undefined["undefined"];
var eq = function (dictEq) {
    return Data_Typelevel_Undefined["undefined"];
};
var boolITrue = new BoolI(function (v) {
    return true;
});
var eqTrueTrue = new Eq(function () {
    return boolITrue;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var impTrueTrue = new Imp(function () {
    return boolITrue;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var orTrueTrue = new Or(function () {
    return boolITrue;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var boolIFalse = new BoolI(function (v) {
    return false;
});
var eqFalseFalse = new Eq(function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
}, function () {
    return boolITrue;
});
var eqFalseTrue = new Eq(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
}, function () {
    return boolIFalse;
});
var eqTrueFalse = new Eq(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var impFalseFalse = new Imp(function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
}, function () {
    return boolITrue;
});
var impFalseTrue = new Imp(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var impTrueFalse = new Imp(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var notFalse = new Not(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
});
var notTrue = new Not(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
});
var orFalseFalse = new Or(function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var orFalseTrue = new Or(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var orTrueFalse = new Or(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
}, function () {
    return boolITrue;
});
var xorFalseFalse = new Xor(function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var xorFalseTrue = new Xor(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var xorTrueFalse = new Xor(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
}, function () {
    return boolITrue;
});
var xorTrueTrue = new Xor(function () {
    return boolITrue;
}, function () {
    return boolITrue;
}, function () {
    return boolIFalse;
});
var boolIBool = function (dictBoolI) {
    return new Bool(function () {
        return dictBoolI;
    });
};
var reifyBool = function (v) {
    return function (f) {
        if (v) {
            return f(boolIBool(boolITrue))(trueT);
        };
        if (!v) {
            return f(boolIBool(boolIFalse))(falseT);
        };
        throw new Error("Failed pattern match at Data.Typelevel.Bool line 63, column 1 - line 63, column 68: " + [ v.constructor.name, f.constructor.name ]);
    };
};
var andTrueTrue = new And(function () {
    return boolITrue;
}, function () {
    return boolITrue;
}, function () {
    return boolITrue;
});
var andTrueFalse = new And(function () {
    return boolITrue;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var andFalseTrue = new And(function () {
    return boolIFalse;
}, function () {
    return boolITrue;
}, function () {
    return boolIFalse;
});
var andFalseFalse = new And(function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
}, function () {
    return boolIFalse;
});
var and = function (dictAnd) {
    return Data_Typelevel_Undefined["undefined"];
};
module.exports = {
    And: And, 
    Bool: Bool, 
    BoolI: BoolI, 
    Eq: Eq, 
    Imp: Imp, 
    Not: Not, 
    Or: Or, 
    Xor: Xor, 
    and: and, 
    eq: eq, 
    falseT: falseT, 
    imp: imp, 
    not: not, 
    or: or, 
    reifyBool: reifyBool, 
    toBool: toBool, 
    trueT: trueT, 
    xor: xor, 
    showTrue: showTrue, 
    showFalse: showFalse, 
    boolIBool: boolIBool, 
    boolITrue: boolITrue, 
    boolIFalse: boolIFalse, 
    notFalse: notFalse, 
    notTrue: notTrue, 
    andFalseFalse: andFalseFalse, 
    andFalseTrue: andFalseTrue, 
    andTrueFalse: andTrueFalse, 
    andTrueTrue: andTrueTrue, 
    orFalseFalse: orFalseFalse, 
    orFalseTrue: orFalseTrue, 
    orTrueFalse: orTrueFalse, 
    orTrueTrue: orTrueTrue, 
    xorFalseFalse: xorFalseFalse, 
    xorFalseTrue: xorFalseTrue, 
    xorTrueFalse: xorTrueFalse, 
    xorTrueTrue: xorTrueTrue, 
    impFalseFalse: impFalseFalse, 
    impFalseTrue: impFalseTrue, 
    impTrueFalse: impTrueFalse, 
    impTrueTrue: impTrueTrue, 
    eqFalseFalse: eqFalseFalse, 
    eqFalseTrue: eqFalseTrue, 
    eqTrueFalse: eqTrueFalse, 
    eqTrueTrue: eqTrueTrue
};
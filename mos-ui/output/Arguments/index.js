// Generated by purs version 0.11.6
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Node_Yargs_Applicative = require("../Node.Yargs.Applicative");
var Node_Yargs_Setup = require("../Node.Yargs.Setup");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var Args = function (x) {
    return x;
};
var argsSetup = Data_Foldable.fold(Data_Foldable.foldableArray)(Node_Yargs_Setup.monoidYargsSetup)([ Node_Yargs_Setup.usage("$0 [-d]"), Node_Yargs_Setup.help("help")("Monerodo OS") ]);
var arg = function (development) {
    return function (monerodService) {
        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)({
            development: development, 
            monerodService: monerodService
        });
    };
};
var args = Node_Yargs_Applicative.runY(argsSetup)(Control_Apply.apply(Node_Yargs_Applicative.applyT)(Data_Functor.map(Node_Yargs_Applicative.functorY)(arg)(Node_Yargs_Applicative.flag("d")([ "development" ])(new Data_Maybe.Just("Open Chrome dev tools"))))(Node_Yargs_Applicative.yarg(Node_Yargs_Applicative.argString)("monerod-service")([  ])(new Data_Maybe.Just("monerod systemd service name"))(new Data_Either.Left("monerod.service"))(false)));
module.exports = {
    Args: Args, 
    arg: arg, 
    args: args, 
    argsSetup: argsSetup
};
// Generated by purs version 0.11.7
"use strict";
var Arguments = require("../Arguments");
var Client = require("../Client");
var Client_Constants = require("../Client.Constants");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans");
var Control_Monad_Trans_Control = require("../Control.Monad.Trans.Control");
var DBus = require("../DBus");
var Data_Argonaut = require("../Data.Argonaut");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Functor_Singleton = require("../Data.Functor.Singleton");
var Data_Semigroup = require("../Data.Semigroup");
var Electron = require("../Electron");
var Node_Process = require("../Node.Process");
var Prelude = require("../Prelude");
var Queue = require("../Queue");
var Queue_Internal = require("../Queue.Internal");
var Types = require("../Types");
var Types_DBus = require("../Types.DBus");
var Types_Env = require("../Types.Env");
var main = function __do() {
    var v = Arguments.args();
    Control_Monad_Eff_Console.log("Starting Monerodo OS User Interface")();
    var v1 = Types_Env.mkEnv(v)();
    Types.runAppM(v1)(Client.monerodoClient(Types.monadApp(Control_Monad_Reader_Trans.monadEffReader(Control_Monad_Eff_Class.monadEffEff))(Control_Monad_Reader_Trans.monadReaderReaderT(Control_Monad_Eff.monadEff))(Control_Monad_Trans_Control.readerTMonadBaseControl(Control_Monad_Trans_Control.effMonadBaseControl)(Control_Monad_Eff.monadEff)(Control_Monad_Eff.monadEff))(Data_Functor_Singleton.singletonFunctorCompose(Data_Functor_Singleton.singletonFunctorIdentity)(Data_Functor_Singleton.singletonFunctorIdentity))))();
    var v2 = Node_Process.cwd();
    return Electron.openWindow({
        file: v2 + "/index.html",
        width: 1024,
        height: 768,
        devTools: v1.development,
        whenLoaded: function (v3) {
            return Queue_Internal.onQueue(v1.signalQueue)(function (x) {
                return v3.send({
                    channel: Client_Constants.signalOutput,
                    message: Data_Argonaut_Encode_Class.encodeJson(Types_DBus.encodeJsonSignalOutput)(x)
                });
            });
        }
    })();
};
module.exports = {
    main: main
};

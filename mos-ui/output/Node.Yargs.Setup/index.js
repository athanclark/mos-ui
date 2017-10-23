// Generated by purs version 0.11.6
"use strict";
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var wrap = function (cols) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y.wrap(cols);
    });
};
var version = function (v) {
    return function (key) {
        return function (desc) {
            return Unsafe_Coerce.unsafeCoerce(function (y) {
                return y.version(v, key, desc);
            });
        };
    };
};
var usage = function (msg) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y.usage(msg);
    });
};
var string = function (key) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y.string(key);
    });
};
var strict = Unsafe_Coerce.unsafeCoerce(function (y) {
    return y.strict();
});
var showHelpOnFail = function (enable) {
    return function (msg) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.showHelpOnFail(enable, msg);
        });
    };
};
var semigroupYargsSetup = new Data_Semigroup.Semigroup(function (s1) {
    return function (s2) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return Unsafe_Coerce.unsafeCoerce(s2)(Unsafe_Coerce.unsafeCoerce(s1)(y));
        });
    };
});
var requiresArg = function (key) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y.requiresArg(key);
    });
};
var monoidYargsSetup = new Data_Monoid.Monoid(function () {
    return semigroupYargsSetup;
}, Unsafe_Coerce.unsafeCoerce(function (y) {
    return y;
}));
var help = function (key) {
    return function (desc) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.help(key, desc);
        });
    };
};
var example = function (cmd) {
    return function (desc) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.example(cmd, desc);
        });
    };
};
var describe = function (key) {
    return function (desc) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.describe(key, desc);
        });
    };
};
var demandCount = function (count) {
    return function (desc) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.demand(count, desc);
        });
    };
};
var demand = function (key) {
    return function (msg) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.demand(key, msg);
        });
    };
};
var defaultVersion = Unsafe_Coerce.unsafeCoerce(function (y) {
    return y.version();
});
var defaultHelp = Unsafe_Coerce.unsafeCoerce(function (y) {
    return y.help();
});
var config = function (key) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y.config(key);
    });
};
var $$boolean = function (key) {
    return Unsafe_Coerce.unsafeCoerce(function (y) {
        return y["boolean"](key);
    });
};
var alias = function (key) {
    return function (a) {
        return Unsafe_Coerce.unsafeCoerce(function (y) {
            return y.alias(key, a);
        });
    };
};
module.exports = {
    alias: alias, 
    "boolean": $$boolean, 
    config: config, 
    defaultHelp: defaultHelp, 
    defaultVersion: defaultVersion, 
    demand: demand, 
    demandCount: demandCount, 
    describe: describe, 
    example: example, 
    help: help, 
    requiresArg: requiresArg, 
    showHelpOnFail: showHelpOnFail, 
    strict: strict, 
    string: string, 
    usage: usage, 
    version: version, 
    wrap: wrap, 
    semigroupYargsSetup: semigroupYargsSetup, 
    monoidYargsSetup: monoidYargsSetup
};

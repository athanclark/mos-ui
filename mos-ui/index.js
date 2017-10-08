// Generated by psc-bundle 0.11.6
var PS = {};
(function(exports) {
    "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  exports["log"] = $foreign.log;
})(PS["Control.Monad.Eff.Console"] = PS["Control.Monad.Eff.Console"] || {});
(function(exports) {
    "use strict";

  exports.runEffFn1 = function runEffFn1(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
})(PS["Control.Monad.Eff.Uncurried"] = PS["Control.Monad.Eff.Uncurried"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Monad.Eff.Uncurried"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  exports["runEffFn1"] = $foreign.runEffFn1;
})(PS["Control.Monad.Eff.Uncurried"] = PS["Control.Monad.Eff.Uncurried"] || {});
(function(exports) {
    "use strict";
  var electron =require("electron"); 
  var app = electron.app;
  var BrowserWindow = electron.BrowserWindow;
  var path =require("path"); 

  var mainWindow;

  exports.openWindowImpl = function openWindowImpl (ps) {
    var createWindow = function createWindow () {
      // Create the browser window.
      mainWindow = new BrowserWindow({width: ps.width, height: ps.height});

      // and load the index.html of the app.
      mainWindow.loadURL(path.join("file://", ps.file));

      // Open the DevTools.
      if (ps.devTools) {
        mainWindow.webContents.openDevTools();
      }

      // Emitted when the window is closed.
      mainWindow.on('closed', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        mainWindow = null;
      });
    };
    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
    // Some APIs can only be used after this event occurs.
    app.on('ready', createWindow);

    // Quit when all windows are closed.
    app.on('window-all-closed', function () {
      // On OS X it is common for applications and their menu bar
      // to stay active until the user quits explicitly with Cmd + Q
      if (process.platform !== 'darwin') {
        app.quit();
      }
    });

    app.on('activate', function () {
      // On OS X it's common to re-create a window in the app when the
      // dock icon is clicked and there are no other windows open.
      if (mainWindow === null) {
        createWindow();
      }
    });

    // In this file you can include the rest of your app's specific main process
    // code. You can also put them in separate files and require them here.
  };
})(PS["Electron"] = PS["Electron"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Electron"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Uncurried = PS["Control.Monad.Eff.Uncurried"];
  var Electron_Types = PS["Electron.Types"];
  var Prelude = PS["Prelude"];
  var React = PS["React"];                            
  var openWindow = Control_Monad_Eff_Uncurried.runEffFn1($foreign.openWindowImpl);
  exports["openWindow"] = openWindow;
})(PS["Electron"] = PS["Electron"] || {});
(function(exports) {
    "use strict";

  exports.process = process;
})(PS["Node.Process"] = PS["Node.Process"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Node.Process"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Control_Monad_Eff_Exception = PS["Control.Monad.Eff.Exception"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Posix = PS["Data.Posix"];
  var Data_Posix_Signal = PS["Data.Posix.Signal"];
  var Data_StrMap = PS["Data.StrMap"];
  var Node_Platform = PS["Node.Platform"];
  var Node_Stream = PS["Node.Stream"];
  var Prelude = PS["Prelude"];
  var Unsafe_Coerce = PS["Unsafe.Coerce"];
  var cwd = $foreign.process.cwd;
  exports["cwd"] = cwd;
})(PS["Node.Process"] = PS["Node.Process"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Electron = PS["Electron"];
  var Node_Process = PS["Node.Process"];
  var Prelude = PS["Prelude"];        
  var main = function __do() {
      Control_Monad_Eff_Console.log("Hello sailor!")();
      var v = Node_Process.cwd();
      return Electron.openWindow({
          file: v + "/index.html", 
          width: 800, 
          height: 600, 
          devTools: false
      })();
  };
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();
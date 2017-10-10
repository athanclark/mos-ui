# Monerodo OS User Interface

This repository holds the code for the user interface / client component of the Monerodo OS suite.

It uses:

- Electron
- React.js
- Material-UI
- PureScript
- Thermite

for its frontend, visual user interface component. It communicates with the daemon through D-Bus,
and invokes remote procedures that way as well.


## Building

### Prerequisites

- PureScript v0.11.6, and that `purs` is in your `$PATH`
- NVM

### Compiling

```bash
# Use the proper version of node for electron v1.7.8
nvm install v7.9.0 && nvm use v7.9.0

# Fetch dependencies
./provision.sh

# Build
./build.sh
```

This should generate a lot of output with warnings and the like, but it _should_ succeed. If there are
any problems, feel free to file an issue.

## Running

You can start the frontend application with `npm start`, or use the executable you built in `./mos-ui-PLATFORM-ARCH/mos-ui`.


# Known Issues

If you see a crazy error along the lines of:

```
...
A JavaScript error occurred in the main process
Uncaught Exception:
Error: The module '/home/athan/dev/mos-ui/mos-ui/node_modules/abstract-socket/build/Release/abstract_socket.node'
was compiled against a different Node.js version using
NODE_MODULE_VERSION 51. This version of Node.js requires
NODE_MODULE_VERSION 54. Please try re-compiling or re-installing
...
```

You'll have to rebuild some libraries. Node makes this really easy, thank goodness:

```bash
npm rebuild --runtime=electron --target=1.7.8 --disturl=https://atom.io/download/atom-shell --build-from-source
./node_modules/.bin/electron-rebuild --version=1.7.8
```

See [this stackoverflow question](https://stackoverflow.com/questions/39547292/node-module-version-mismatch-expected-50)
for more details.

Running `npm start` _should_ now work. Again, if you experience any issues, please let me know.

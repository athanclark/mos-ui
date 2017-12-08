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


## Usage

```bash
npm start -- [--help] [--development]
# or
./mos-ui-linux-x64/mos-ui -- [--help] [--development]
```


## Building

### Prerequisites

- NVM

### Compiling

```bash
# Use the proper version of node for electron v1.7.8
nvm install v7.9.0 && nvm use v7.9.0
npm install
```

This should generate a lot of output with warnings and the like, but it _should_ succeed. If there are
any problems, feel free to file an issue.

## Running

You can start the frontend application with `npm start`, or use the executable you built in `./mos-ui-PLATFORM-ARCH/mos-ui`.

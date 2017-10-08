#! /bin/bash

pulp build && \
    purs bundle output/**/*.js -m Spec --main Spec > frontend.tmp.js && \
    ./node_modules/.bin/browserify frontend.tmp.js > frontend.js && \
    rm frontend.tmp.js && \
    purs bundle output/**/*.js -m Main --main Main > index.js && \
    ./node_modules/.bin/electron-packager . --overwrite

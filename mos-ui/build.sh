#! /bin/bash

./node_modules/.bin/pulp build && \
    purs bundle output/**/*.js -m Spec --main Spec > frontend.js && \
    purs bundle output/**/*.js -m Main --main Main > index.js && \
    ./node_modules/.bin/electron-packager . --overwrite

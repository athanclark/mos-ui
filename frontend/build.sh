#! /bin/bash

pulp build && \
    psc-bundle output/**/*.js -m Main --main Main -o dist/app.unbrowserified.js && \
    ./node_modules/.bin/browserify dist/app.unbrowserified.js > dist/app.js && \
    ./node_modules/.bin/uglifyjs dist/app.js > dist/app.min.js

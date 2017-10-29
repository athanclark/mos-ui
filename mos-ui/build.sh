#! /bin/bash

PATH=${PATH}:./node_modules/.bin ./node_modules/.bin/pulp build && \
    # purs bundle output/**/*.js -m Spec --main Spec > frontend.js && \
    # purs bundle output/**/*.js -m Main --main Main > index.js && \
    ./node_modules/.bin/electron-packager . --overwrite && \
    npm rebuild --runtime=electron --target=1.7.9 --disturl=https://atom.io/download/atom-shell --build-from-source

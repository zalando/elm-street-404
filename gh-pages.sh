#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir gh-pages

# compile JS using Elm
elm make src/Main.elm --output gh-pages/elm.js

# copy the images and html
cp -R index.html catwatch.png embed.html img gh-pages

cd gh-pages

# minify js
uglifyjs --compress warnings=false --mangle --output elm.js -- elm.js

# init branch and commit
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:zalando/elm-street-404.git" master:gh-pages

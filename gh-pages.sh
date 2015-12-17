#!/bin/bash
set -e

rm -rf gh-pages || exit 0;
mkdir gh-pages;

# compile the files using Elm
elm make Main.elm --output gh-pages/main.html
html-minifier --minify-js --minify-js gh-pages/main.html -o gh-pages/index.html

# Copy the images
cp -R img gh-pages

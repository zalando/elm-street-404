#!/bin/bash
set -e

rm -rf gh-pages || exit 0;
mkdir gh-pages;

# compile the files using Elm
elm make Main.elm --output gh-pages/index.html

# Copy the images
cp -R img gh-pages

cd gh-pages

git init
git config user.name "travis"
git add .
git commit -m "Deploying to GH Pages"

git push --force --quiet "https://${GH_TOKEN}@github.com/zalando/elm-street-404.git" master:gh-pages > /dev/null 2>&1

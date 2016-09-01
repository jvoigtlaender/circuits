rm -rf out || exit 0;

elm-package install --yes
elm-make --yes

mkdir out
mv src/index.html out
elm-make src/Main.elm --output out/elm.js --yes
cd out

git init
git config user.name "Travis CI"
git config user.email "jvoigtlaender@users.noreply.github.com"
git add .
git commit -m "Travis deploy to gh-pages"
git push --force --quiet "https://$GH_TOKEN@github.com/$TRAVIS_REPO_SLUG" master:gh-pages >/dev/null 2>&1
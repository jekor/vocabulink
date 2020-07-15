#!/usr/bin/env bash

set -Eeuo pipefail

# shellcheck source=/dev/null
source "${stdenv:?}/setup"

cp -r "${src:?}" src
chmod -R u+w src
cd src

patchPhase

spritesheets="icon markitup toast"

declare -A jses=(
  [common]="$jquery $cookie $loadmask $reveal $minform common.js toast.js loggedout.js"
  [link]="$longtable link.js"
  [member]=" $showdown jquery.markitup.js markitup.set.js loggedin.js comment.js"
  [dashboard]="$drcal dashboard.js"
  [review]="$hotkeys $easing review.js"
  [member-page]="member-page.js"
  [reader]="reader.js"
)

declare -A csses=(
  [common]="common.styl comment.styl toast.styl loadmask.styl reveal.styl $out/img/icon.css $out/img/toast.css"
  [member]="markitup.styl $out/img/markitup.css"
  [link]="link.styl"
  [article]="article.styl"
  [dashboard]="dashboard.styl"
  [member-page]="member-page.styl"
  [front]="front.styl"
  [review]="review.styl"
  [reader]="reader.styl"
)

mkdir -p "$out/js" "$out/img" "$out/css" "$out/articles"

# JavaScript
pushd js

for k in "${!jses[@]}"; do
  uglifyjs ${jses[$k]} --mangle -o "$out/js/$k.js"
done

popd

# Images
pushd img

cp -r ./*.png ./*.gif off-the-shelf reader "$out/img/"

# TODO: Create an img manifest so that CSS files can cache-bust images.

## Spritesheets

### MarkItUp! sprites are famfamfam Silk icons.
mkdir markitup && pushd markitup
unzip "${silkicons:?}"
for icon in text_heading_1 text_heading_2 text_heading_3 text_bold text_italic text_list_bullets text_list_numbers picture link user_comment script_code tick; do
  mv "icons/$icon.png" .;
done
rm -rf icons readme.html readme.txt
popd

for spritesheet in $spritesheets; do
  glue "$spritesheet" "$out/img" --url=/img/ --cachebuster --crop
done

pngs=$(find "$out/img" -iname "*.png")
chmod +w "$pngs" "$out/img/off-the-shelf" && optipng -clobber "$pngs"

popd

# CSS
pushd css

for k in "${!csses[@]}"; do
  args=""
  for f in ${csses[$k]}; do
    if [ "${f: -5}" == ".styl" ]; then
      args+=" <(stylus < $f)"
    else
      args+=" $f"
    fi
  done
  eval "cat $args > $out/css/$k.css"
done

for spritesheet in $spritesheets; do
  rm "$out/img/$spritesheet.css"
done

## Some CSS comes from a theme I cannot redistribute.
cp off-the-shelf.css "$out/css/"

popd

# Documents
pushd articles

for article in *.markdown; do
  pandoc --section-divs --mathjax -t html5+smart --toc --standalone --template=template.html < "$article" > "$out/articles/${article%.markdown}.html"
done

popd

# Audio

## These are not under revision control.
if [ -d audio ]; then
  cp -r audio "$out/"
fi

# Store a manifest of the static assets (js/css/img).
# We can use this manifest for cache busting.
pushd "$out"

find js css img -type f -print0 | xargs -0 sha1sum > manifest

popd

source $stdenv/setup

cp -r $src src
chmod -R u+w src
cd src

patchPhase

spritesheets="icon markitup toast"

declare -A jses=(
    [common]="$jquery $cookie $loadmask $reveal $minform common.js toast.js loggedout.js"
    [link]="$longtable link.js"
    [member]="$markitup/markitup/jquery.markitup.js $markitup/markitup/sets/default/set.js $showdown loggedin.js comment.js"
    [dashboard]="$drcal dashboard.js"
    [learn]="$hotkeys $easing learn.js"
    [member-page]="member-page.js"
    [reader]="reader.js"
)

declare -A csses=(
    [common]="common.styl comment.styl toast.styl loadmask.styl reveal.styl $out/img/icon.css $out/img/toast.css"
    [member]="markitup.styl $out/img/markitup.css"
    [article]="article.styl"
    [dashboard]="dashboard.styl"
    [member-page]="member-page.styl"
    [front]="front.styl"
    [review]="review.styl"
    [reader]="reader.styl"
)

mkdir -p $out/js $out/img $out/css $out/articles

# JavaScript
pushd js

for k in "${!jses[@]}"; do
    uglifyjs ${jses[$k]} --mangle --screw-ie8 -o $out/js/$k.js
done

popd

# Images
pushd img

cp -r *.png *.gif off-the-shelf reader $out/img/

## Spritesheets
for spritesheet in $spritesheets; do
    glue $spritesheet $out/img --url=/img/ --cachebuster --crop
done

pngs=$(find $out/img -iname "*.png")
chmod +w $pngs $out/img/off-the-shelf && optipng -clobber $pngs

popd

# CSS
pushd css

for k in "${!csses[@]}"; do
    args=""
    for f in ${csses[$k]}; do
        if [ ${f: -5} == ".styl" ]; then
            args+=" <(stylus < $f)"
        else
            args+=" $f"
        fi
    done
    eval "cat $args > $out/css/$k.css"
done

for spritesheet in $spritesheets; do
    rm $out/img/$spritesheet.css
done

## Some CSS comes from a theme I cannot redistribute.
cp off-the-shelf.css $out/css/

popd

# Documents
pushd articles

for article in *.markdown; do
    pandoc --smart --section-divs --mathjax -t html5 --toc --standalone --template=template.html < "$article" > $out/articles/$(basename "$article" .markdown).html
done

popd

# Audio

## These are not under revision control.
if [ -d audio ]; then
    cp -r audio $out/
fi

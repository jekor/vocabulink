#!/bin/bash

linkno=$1
language=$2
word=$3
url="http://apifree.forvo.com/key/${FORVO_KEY}/format/json/action/word-pronunciations/word/$word/language/$language/order/rate-desc"
resp=`curl $url`
curl `echo $resp | jsawk 'return this.items[0].pathmp3'` -o ${linkno}.mp3
curl `echo $resp | jsawk 'return this.items[0].pathogg'` -o ${linkno}.ogg

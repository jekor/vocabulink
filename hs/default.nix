{ mkDerivation, aeson, aeson-qq, applicative-extras, base
, blaze-builder, blaze-html, blaze-markup, bytestring, cgi
, ConfigFile, containers, convertible, cookie, data-default
, directory, either, filepath, hashmap, MissingH, MonadCatchIO-mtl
, mtl, network, network-uri, old-time, pandoc, parsec
, process, pureMD5, random, SHA, split, sscgi, stdenv, syb
, template-haskell, templatepg, text, time, tuple, unix
, utf8-string, utility-ht, vector
, postgresql, vocabulink-sql, domain
}:
mkDerivation {
  pname = "Vocabulink";
  version = "2015.12.15";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-qq applicative-extras base blaze-builder blaze-html
    blaze-markup bytestring cgi ConfigFile containers convertible
    cookie data-default directory either filepath hashmap MissingH
    MonadCatchIO-mtl mtl network network-uri old-time pandoc
    parsec process pureMD5 random SHA split sscgi syb
    template-haskell templatepg text time tuple unix utf8-string
    utility-ht vector
  ];
  # Allow overriding domain (for testing, i.e. vocabulink.com -> vocabulink.lan).
  postPatch = ''
    for f in $(find . -type f); do sed -i -e 's/vocabulink\.com/${domain}/g' "$f"; done
  '';
  preBuild = ''
    # Setup a PostgreSQL database for templatepg compile-time type inference.
    ${postgresql}/bin/initdb db
    ${postgresql}/bin/postgres -D db &
    ppid=$!
    sleep 5
    ${postgresql}/bin/psql -f ${vocabulink-sql} template1
    export TPG_DB="vocabulink"
    export TPG_USER="$(whoami)"
  '';
  postBuild = ''
    kill $ppid
  '';
  homepage = "http://www.vocabulink.com/source";
  description = "a web application for learning languages through fiction (mnemonics)";
  license = "unknown";
}

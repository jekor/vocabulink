{ mkDerivation, aeson, aeson-qq, applicative-extras, base
, blaze-builder, blaze-html, blaze-markup, bytestring, cgi
, ConfigFile, containers, convertible, cookie, data-default
, directory, either, filepath, hashmap, MissingH, MonadCatchIO-mtl
, mtl, network, network-uri, old-locale, old-time, pandoc, parsec
, process, pureMD5, random, SHA, split, sscgi, stdenv, syb
, template-haskell, templatepg, text, time, tuple, unix
, utf8-string, utility-ht, vector
, auth-token-key, db-password, postgresql, vocabulink-sql
}:
mkDerivation {
  pname = "Vocabulink";
  version = "2013.5.16";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-qq applicative-extras base blaze-builder blaze-html
    blaze-markup bytestring cgi ConfigFile containers convertible
    cookie data-default directory either filepath hashmap MissingH
    MonadCatchIO-mtl mtl network network-uri old-locale old-time pandoc
    parsec process pureMD5 random SHA split sscgi syb
    template-haskell templatepg text time tuple unix utf8-string
    utility-ht vector
  ];
  preBuild = ''
    # Setup a PostgreSQL database for templatepg compile-time type inference.
    ${postgresql}/bin/initdb db
    ${postgresql}/bin/postgres -D db &
    ppid=$!
    sleep 5
    ${postgresql}/bin/createdb vocabulink
    ${postgresql}/bin/psql -f ${vocabulink-sql} vocabulink
    export TPG_DB="vocabulink"
    export TPG_USER="$(whoami)"

    export db_password="${db-password}"
    export auth_token_key="${auth-token-key}"
  '';
  postBuild = ''
    kill $ppid
  '';
  homepage = "http://www.vocabulink.com/source";
  description = "a web application for learning languages through fiction (mnemonics)";
  license = "unknown";
}

{ mkDerivation
, aeson
, aeson-qq
, applicative-extras
, base
, blaze-builder
, blaze-html
, blaze-markup
, bytestring
, cabal-install
, cgi
, ConfigFile
, containers
, convertible
, cookie
, data-default
, directory
, either
, exceptions
, filepath
, hashmap
, MissingH
, mtl
, network
, network-uri
, old-time
, pandoc
, parsec
, postgresql-typed
, process
, pureMD5
, random
, SHA
, split
, sscgi
, stdenv
, syb
, template-haskell
, text
, time
, tuple
, unix
, utf8-string
, utility-ht
, vector
, postgresql
, vocabulink-sql
, domain
, static-manifest
}:
mkDerivation rec {
  pname = "Vocabulink";
  version = "2015.12.15";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    aeson-qq
    applicative-extras
    base
    blaze-builder
    blaze-html
    blaze-markup
    bytestring
    cgi
    ConfigFile
    containers
    convertible
    cookie
    data-default
    directory
    either
    exceptions
    filepath
    hashmap
    MissingH
    mtl
    network
    network-uri
    old-time
    pandoc
    parsec
    postgresql-typed
    process
    pureMD5
    random
    SHA
    split
    sscgi
    syb
    template-haskell
    text
    time
    tuple
    unix
    utf8-string
    utility-ht
    vector
  ];
  buildDepends = [
    cabal-install
  ];
  # Allow overriding domain (for testing, i.e. vocabulink.com -> vocabulink.lan).
  postPatch = ''
    for f in $(find . -type f); do sed -i -e 's/vocabulink\.com/${domain}/g' "$f"; done
  '';
  preBuild = ''
    # Setup a PostgreSQL database for compile-time type inference.
    dbdir=$(mktemp -d)
    ${postgresql}/bin/initdb "$dbdir"
    ${postgresql}/bin/postgres -D "$dbdir" -c listen_addresses="" -c unix_socket_directories="$dbdir" &
    ppid=$!
    sleep 1
    # The output from psql here interferes with Emacs dante mode, so sending it to /dev/null.
    (${postgresql}/bin/psql -h "$dbdir" -f ${vocabulink-sql} template1 > /dev/null)
    export TPG_DB="vocabulink"
    export TPG_USER="$(whoami)"
    export TPG_SOCK="$dbdir/.s.PGSQL.5432"
    export MANIFEST="${static-manifest}"

    echo -e 'module Vocabulink.Constants where\n' > Vocabulink/Constants.hs
    echo -e 'import qualified Data.Map.Strict as M\n' >> Vocabulink/Constants.hs
    ${postgresql}/bin/psql -h "$dbdir" -U vocabulink vocabulink -c 'COPY (SELECT languages_haskell()) TO STDOUT' >> Vocabulink/Constants.hs

    trap "kill $ppid && rm -rf $dbdir" EXIT
  '';
  postBuild = ''
    kill $ppid
  '';
  shellHook = preBuild;
  homepage = "http://www.vocabulink.com/source";
  description = "a web application for learning languages through fiction (mnemonics)";
  license = "unknown";
}

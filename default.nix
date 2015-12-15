{ stdenv, fetchurl, fetchgit, bash, coreutils, findutils, pandoc
, uglify, stylus, glue, optipng }:

stdenv.mkDerivation {
  name = "vocabulink";
  src = ./.;
  minform = fetchurl {
    url = "https://raw.githubusercontent.com/jekor/minform/a8b8d00960f1351a9f1947a14dbe596f572d9f11/minform.js";
    sha256 = "13s6ccacfvabl81cnx8msks2nmxasna9j1hj1cyx9k46ivwf1gwm";
  };
  longtable = fetchurl {
    url = "https://raw.githubusercontent.com/jekor/longtable/cc9b93c92db71d2ef2db624e5cee0cbdcdc7e823/longtable.js";
    sha256 = "0chfj0gsgvvw8ggqjqxpfra07ng3rpwcqhcwwlkh3064kviapnc7";
  };
  drcal = fetchurl {
    url = "https://raw.githubusercontent.com/jekor/drcal/930dfb4746c85c76eb71f8ed65cda3d27adaf28e/drcal.js";
    sha256 = "1850ljdf4x9xh34514cqbcnmng13wxs5334q0yl9nvna2vg96ffi";
  };
  jquery = fetchurl {
    url = "https://raw.githubusercontent.com/jquery/jquery/8c1ea08ae01a676ea442d5e880ac7b01bed1434b/jquery.js";
    sha256 = "05i0x21n3lgmqg185awzywfqdqdzfvikzlv4nwg7lv1sb2lpdvqf";
  };
  cookie = fetchurl {
    url = "https://raw.githubusercontent.com/carhartl/jquery-cookie/df3e07adc1e1196c7e7d7edf450c488ef1464224/jquery.cookie.js";
    sha256 = "0wgzdrgwyvlzxn8hl4fr17sl788wpqhnznbajp89ifw9x0cpcid0";
  };
  loadmask = fetchurl {
    url = "https://jquery-loadmask.googlecode.com/svn/trunk/src/jquery.loadmask.js";
    sha256 = "0jwrcv5zjmirpr6cp9nc6zbwpqq7h3vjad4yixsmayc63mx06vcr";
  };
  reveal = fetchurl {
    url = "https://raw.githubusercontent.com/zurb/reveal/007e38d6a89934c7ef41d0516759fa26c0aa4b11/jquery.reveal.js";
    sha256 = "0w1sv810r9z2vkdkh22gk8mcaqxac82k22b898przx0q9kgxmkkz";
  };
  markitup = fetchgit {
    url = "https://github.com/markitup/1.x.git";
    rev = "3c736e5a3ee0836ed8ca67c72beb9dfce102b51b";
    sha256 = "105d280c6a958a9b2ca7a383bc28e84aa7d804bc8edc68a77fe2581d57333294";
  };
  showdown = fetchurl {
    url = "https://raw.githubusercontent.com/showdownjs/showdown/33e6d6709683432c36b80c8e5b954b14355dc31e/src/showdown.js";
    sha256 = "04db4dh9xgqm2rm1zwil5k70fq2yz7lcd492ig46a5klmapmldh3";
  };
  hotkeys = fetchurl {
    url = "https://raw.githubusercontent.com/jeresig/jquery.hotkeys/0451de18d57d3401bd4cc021facbe5fd63b5aae6/jquery.hotkeys.js";
    sha256 = "0w0gldwmgw6snzwhnfn1zlqcr7psw9dcww5ck4p8pw15n7k5lhjy";
  };
  easing = fetchurl {
    url = "https://raw.githubusercontent.com/gdsmith/jquery.easing/0ea4f96be8b46215ec63a84108ceec74ef3374f2/jquery.easing.1.3.js";
    sha256 = "0d6xi10mxpcb5s0ak188i1flgxqcbqnd9qm548cmhsjzdvzzfmq7";
  };
  buildInputs = [ bash uglify stylus glue optipng pandoc ];
  inherit bash coreutils findutils;
  builder = ./builder.sh;
}

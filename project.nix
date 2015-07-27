{ mkDerivation, base, containers, configurator, parsec, pretty
, pretty-show, stdenv, text, time, lens
}:
mkDerivation {
  pname = "scrollkeeper";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base containers configurator parsec pretty pretty-show text time
    lens
  ];
  homepage = "http://memorici.de";
  description = "Aggregate and transform time tracker data";
  license = stdenv.lib.licenses.mit;
}

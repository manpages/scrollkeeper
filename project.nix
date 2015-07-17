{ mkDerivation, base, configurator, parsec, pretty, pretty-show
, stdenv, text, time
}:
mkDerivation {
  pname = "scrollkeeper";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base configurator parsec pretty pretty-show text time
  ];
  homepage = "http://memorici.de";
  description = "Aggregate and transform time tracker data";
  license = stdenv.lib.licenses.mit;
}

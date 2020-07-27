with import <nixpkgs> {}; {
  qpidEnv = stdenv.mkDerivation {
    name = "fv3";
    buildInputs = [
        netcdf
    ];
}


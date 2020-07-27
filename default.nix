with import <nixpkgs> {};
  pkgs.mkShell{
    name = "fv3";
    buildInputs = [
        netcdf
    ];
}

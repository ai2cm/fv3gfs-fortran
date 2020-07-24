let
  pkgs = import <nixpkgs> {};
  fms=  import ./nix/fms;
  nceplibs=  import ./nix/nceplibs;
      
in
  pkgs.mkShell {
    buildInputs = [
      fms nceplibs pkgs.gfortran
    ];


    FMS_DIR="${fms}/include";
    ESMF_DIR="${fms}";
    LD_LIBRARY_PATH="$${LD_LIBRARY_PATH}:$${ESMF_DIR}/lib/libO3/Linux.gfortran.64.mpiuni.default/:${fms}/libFMS/.libs/:$${SERIALBOX_DIR}/lib";
    FFLAGS="-I${fms}/include";
}

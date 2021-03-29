let 
  nixpkgs = import ./.;
in
  with nixpkgs;
  mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = [ fv3 python3Packages.tox ];
}

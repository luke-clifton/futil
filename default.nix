let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.stdenv.mkDerivation {
    name = "futil";
    src = ./.;
    buildInputs = [nixpkgs.skalibs];
  }

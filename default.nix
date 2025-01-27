{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "end-of-exe";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base pkgs.haskellPackages.directory ];
  homepage = "https://hackage.haskell.org/package/end-of-exe";
  description = "A small library to deal with executable endings";
  license = pkgs.lib.licenses.mit;
}

{ pkgs ? import <nixpkgs> {} }:
let

  derivations =
    pkgs.fetchFromGitHub {
      owner = "earldouglas";
      repo = "derivations";
      rev = "007b06268778bc86def0f77f986556789db6e539";
      sha256 = "0k34hbvmvc24gwjkc680bm45ss0bv5dklahgr8adqkg46zi3kkwd";
    };

  nvim-haskell = import (derivations + "/nvim/haskell") {};

  haskell =
    pkgs.haskellPackages.ghcWithPackages (pkgs: [
      pkgs.cabal-install
      pkgs.zlib
    ]);
in
pkgs.mkShell {
  nativeBuildInputs = [
    nvim-haskell
    haskell
    pkgs.haskell-language-server
  ];
}

let
  sources = import ./sources.nix;

  nixpkgs = import sources.nixpkgs {};
  niv = import sources.niv {};
in
niv // nixpkgs

with import ./nix/packages.nix;

stdenv.mkDerivation {
  name = "dicey";
  buildInputs = [
    gnumake
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    nodePackages.uglify-js
  ];
  src = ./.;
  preBuild = ''
    export HOME=$TMP # Make elm happy
    make clean
  '';
  makeFlags = [ "ENVIRONMENT=production" ];
  installPhase = "cp -r dist $out";
}

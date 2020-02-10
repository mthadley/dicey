let
  pkgs = import ./nix/packages.nix;
in
pkgs.mkShell {
  inputsFrom = [ (import ./.) ];
  buildInputs = with pkgs; [
    entr
    niv
  ];
}

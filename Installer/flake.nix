{
  description = "Flake for the nuros-install";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs { inherit system overlays; };

        project = pkgs.haskell-nix.stackProject {
          src = ./.;
          stackYaml = "stack.yaml";
        };
      in
      {
        packages = {
          nuros-start = project.packages."nuros-install";
          default = project.packages."nuros-install";
        };

        devShell = pkgs.mkShell {
          inputsFrom = [ project.shell ];

          buildInputs = with pkgs; [
            dosfstools
            util-linux
          ];

          shellHook = ''
            echo "Use: stack build | stack repl | ghci"
          '';
        };
      });
}

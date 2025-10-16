{
  description = "Lightweight flake for the nuros-start Stack-based Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskellPackages;
        pname = "nuros-start";
      in
      {
        packages.${pname} = hsPkgs.callCabal2nix pname ./. { };
        packages.default = self.packages.${system}.${pname};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            stack
            pkg-config

            e2fsprogs
            dosfstools
            xfsprogs
            btrfs-progs
            f2fs-tools
            util-linux
          ];

          shellHook = ''
            echo "🧠 Nix dev shell for nuros-start"
            echo "Use: stack build | cabal build | ghci"
          '';
        };
      });
}

{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    packages.${system} = {
      enzo = pkgs.stdenv.mkDerivation {
        name = "enzo";
        src = ./src;
        nativeBuildInputs = [ pkgs.ghc ];
        buildPhase = "ghc Main";
        installPhase = "ghc Main -o $out";
      };
      default = self.packages.${system}.enzo;
    };

    apps.${system} = {
      enzo = {
          type = "app";
          program = "${self.packages.${system}.enzo}";
      };
      default = self.apps.${system}.enzo;
    };

    devShells.${system} = {
      enzo = pkgs.mkShell {
        packages = [ pkgs.ghc ];
      };
      default = self.devShells.${system}.enzo;
    };
  };
}

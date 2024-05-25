{
  description = "ganache";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-overlay.url = "github:evanrelf/haskell-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            overlays = [
              inputs.haskell-overlay.overlay
              (import ./nix/overlays/haskell-packages.nix)
            ];
          };

        packages = {
          default = config.packages.ganache;

          ganache = pkgs.haskellPackages.ganache;
        };

        devShells.default =
          pkgs.mkShell {
            inputsFrom = [ config.packages.ganache.env ];
            packages = [
              pkgs.cabal-install
              pkgs.ghcid
              pkgs.nixpkgs-fmt
            ];
          };
      };
    };
}

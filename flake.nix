{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    devshell = {
      url = "github:deemp/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lima = {
      url = "github:deemp/lima";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.devshell.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        let
          ghcVersion = "910";
          bash.vars = ''
            export LC_ALL=C.UTF-8
          '';

          hpkgs = pkgs.haskell.packages."ghc${ghcVersion}";
          hpkgsFinal = config.haskellProjects.default.outputs.finalPackages;

          # Our only Haskell project. You can have multiple projects, but this template
          # has only one.
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # To avoid unnecessary rebuilds, we filter projectRoot:
            # https://community.flake.parts/haskell-flake/local#rebuild
            projectRoot = builtins.toString (
              lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./docs
                  ./example
                  ./servant-queryparam-client
                  ./servant-queryparam-core
                  ./servant-queryparam-openapi3
                  ./servant-queryparam-server
                  ./cabal.project
                ];
              }
            );

            basePackages = hpkgs.override {
              overrides = final: prev: {
                lima = prev.callCabal2nix "lima" (inputs.lima.outPath + "/lima") { };
              };
            };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
              tools = hp: {
                hlint = null;
                ghcid = null;
                cabal-install = null;
                haskell-language-server = null;
              };
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              hlint.enable = true;
              shellcheck.enable = true;
              fourmolu = {
                enable = true;
              };
            };
          };

          packages = {
            inherit (hpkgsFinal)
              servant-queryparam-core
              servant-queryparam-client
              servant-queryparam-openapi3
              servant-queryparam-server
              ;
          };

          legacyPackages = {
            genDocs = pkgs.writeShellApplication {
              name = "genDocs";
              text = "cd docs && ${lib.getExe hpkgsFinal.docs}";
            };
          };

          # Default shell
          devshells.default = {
            packagesFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];
            bash.extra = bash.vars;
            commandGroups = {

              tools = [
                pkgs.hpack
                pkgs.cabal-install
                hpkgs.haskell-language-server
              ];

            };
          };
        in
        {
          inherit
            haskellProjects
            treefmt
            packages
            devshells
            legacyPackages
            ;
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
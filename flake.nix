{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    lima_.url = "github:deemp/flakes?dir=source-flake/lima";
    lima.follows = "lima_/lima";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # We're going to make some dev tools for our Haskell package
      # The NixOS wiki has more info - https://nixos.wiki/wiki/Haskell

      # --- Imports ---

      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.codium.configs.${system}) extensions settingsNix;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs.workflows.functions.${system}) writeWorkflow;
      inherit (inputs.workflows.configs.${system}) nixCI;
      inherit (inputs) lima;

      # --- Parameters ---

      # The desired GHC version
      ghcVersion = "927";

      # The name of a package
      servant-queryparam-core = "servant-queryparam-core";
      servant-queryparam-client = "servant-queryparam-client";
      servant-queryparam-server = "servant-queryparam-server";
      servant-queryparam-example = "servant-queryparam-example";
      servant-queryparam-openapi3 = "servant-queryparam-openapi3";

      # --- Override ---

      # We need to prepare an attrset of Haskell packages and include our packages into it,
      # so we define an override - https://nixos.wiki/wiki/Haskell#Overrides.
      # We'll supply the necessary dependencies to our packages.
      # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/.
      # For doing that, we use several helper functions.
      # Overriding the packages may trigger multiple rebuilds,
      # so we override as few packages as possible.

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        overrideCabal
        markUnbroken
        ;

      # Here's our override
      # Haskell overrides are described here: https://nixos.org/manual/nixpkgs/unstable/#haskell
      override = {
        overrides = self: super: {
          openapi3 = markUnbroken (dontCheck super.openapi3);
          servant-queryparam-core = self.callCabal2nix servant-queryparam-core ./${servant-queryparam-core} { };
          servant-queryparam-client = self.callCabal2nix servant-queryparam-client ./${servant-queryparam-client} { inherit (self) servant-queryparam-core; };
          servant-queryparam-server = self.callCabal2nix servant-queryparam-server ./${servant-queryparam-server} { inherit (self) servant-queryparam-core; };
          servant-queryparam-openapi3 = self.callCabal2nix servant-queryparam-openapi3 ./${servant-queryparam-openapi3} { inherit (self) servant-queryparam-core; };
          servant-queryparam-example = self.callCabal2nix servant-queryparam-example ./${servant-queryparam-example} { inherit (self) servant-queryparam-core servant-queryparam-server servant-queryparam-openapi3; };
        };
      };

      # --- Haskell tools ---

      # We call a helper function that will give us tools for Haskell
      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        # runtimeDependencies = packageRuntimeDependencies;
        # If we work on multiple packages, we need to supply all of them.
        # Suppose we develop packages A and B, where B is in deps of A.
        # GHC will be given dependencies of both A and B.
        # However, we don't want B to be in the list of deps of GHC
        # because build of GHC may fail due to errors in B.
        packages = (ps: __attrValues {
          inherit (ps)
            servant-queryparam-core
            servant-queryparam-client
            servant-queryparam-server
            servant-queryparam-openapi3
            servant-queryparam-example;
        });
      })
        hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack ghc;

      # --- Tools ---

      # We list the tools that we'd like to use
      tools = [
        hpack
        cabal
        # `cabal` already has a `ghc` on its `PATH`,
        # so you may remove `ghc` from this list
        # Then, you can access `ghc` via `cabal repl` -> `ghci>:! ghc`
        # TODO cabal exec
        ghc
        # anyway, if you'd like to use `GHC`, write it before `HLS` - see https://github.com/NixOS/nixpkgs/issues/225895
        hls
        haskellPackages.cabal-fmt
      ];

      # --- Packages ---

      packages = {
        # --- Haskell packages ---

        inherit (haskellPackages)
          servant-queryparam-core
          servant-queryparam-client
          servant-queryparam-server
          servant-queryparam-openapi
          servant-queryparam-example;

        # --- IDE ---

        # This part can be removed if you don't use `VSCodium`
        # We compose `VSCodium` with dev tools and `HLS`
        # This is to let `VSCodium` run on its own, outside of a devshell
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };

        # a script to write `.vscode/settings.json`
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };

        # --- Flakes ---

        # Scripts that can be used in CI
        inherit (mkFlakesTools [ "." ]) updateLocks pushToCachix;

        # --- GH Actions

        # A script to write GitHub Actions workflow file into `.github/ci.yaml`
        writeWorkflows = writeWorkflow "ci" nixCI;
      };

      # --- Devshells ---

      devShells = {
        default = mkShell {
          packages = tools;
          # sometimes necessary for programs that work with files
          bash.extra = "export LANG=C.utf8";
          commands =
            mkCommands "tools" tools
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
            ++ mkRunCommands "infra" { inherit (packages) writeWorkflows updateLocks pushToCachix; };
        };
      };
    in
    {
      inherit packages devShells;
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}

# servant-queryparam

Provides several libraries that let you use records to specify query parameters in [servant](https://hackage.haskell.org/package/servant) APIs.

These libraries are:

- [servant-queryparam-core](./servant-queryparam-core)
- [servant-queryparam-server](./servant-queryparam-server)
- [servant-queryparam-client](./servant-queryparam-client)
- [servant-queryparam-openapi3](./servant-queryparam-openapi3)

An example of a server:

- [servant-queryparam-core-example](./servant-queryparam-example)

## Build

You can build all packages using `cabal-install`.

```console
cabal build all
```

## Nix flake

This repository provides a `Nix` flake with development tools and Nix derivations of `servant-queryparam-*` libraries.

### Prerequisites

<details>

  <summary>Spoiler</summary>

- [flake.nix](./flake.nix) - code in this flake is extensively commented.
- [language-tools/haskell](https://github.com/deemp/flakes/blob/main/language-tools/haskell/flake.nix) - a flake that conveniently provides `Haskell` tools.
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md#dev-tools) - I recommended to use this flake just for development. For packaging an app, make another flake with a limited number of inputs to reduce the `flake.lock` size.

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` with extensions.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
  - Shows how to build a static binary from your package and how to make a Docker image with it.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md) - general info about `Haskell` tools.
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Nixpkgs support for incremental Haskell builds](https://www.haskellforall.com/2022/12/nixpkgs-support-for-incremental-haskell.html)
- [flakes](https://github.com/deemp/flakes#readme) - my Nix flakes that may be useful for you.

</details>

### Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and build all packages.

    ```console
    nix develop
    cabal build all
    ```

1. (Optionally) Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. (Optionally) Open a `Haskell` file and hover over a function.

1. (Optionally) Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Configs

- [package.yaml](./package.yaml) - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.

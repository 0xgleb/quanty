{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    devenv.url = "github:cachix/devenv/v1.7";
    devenv.inputs = {
      nixpkgs.follows = "nixpkgs";
      git-hooks.follows = "git-hooks";
    };

    ghc-wasm-meta.url =
      "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, git-hooks, devenv, ghc-wasm-meta, ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hooks = {
          # Nix
          nil.enable = true;
          nixfmt-classic.enable = true;

          # Haskell
          fourmolu.enable = true;
          hlint.enable = true;

          # TypeScript
          eslint.enable = true;
          prettier.enable = true;

          # Misc
          denofmt = {
            enable = true;
            excludes = [ ".*\\.ts$" ".*\\.js$" ".*\\.json$" ];
          };
          shellcheck.enable = true;
        };

        devShell = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [{
            # https://devenv.sh/reference/options/
            packages = (with pkgs.haskellPackages; [
              haskell-language-server
              fourmolu
              hlint
            ]) ++ [ ghc-wasm-meta.packages.${system}.all_9_12 ];

            languages = {
              nix.enable = true;
              haskell.enable = true;
              javascript.enable = true;
              javascript.pnpm.enable = true;
              typescript.enable = true;
            };

            env = { };
            git-hooks = { inherit hooks; };
            difftastic.enable = true;
            cachix.enable = true;
          }];
        };

      in {
        devShells.default = devShell;

        checks.git-hooks = git-hooks.lib.${system}.run {
          inherit hooks;
          src = self;
        };

        packages = {
          devenv-up = devShell.config.procfileScript;
          default = devShell.config.procfileScript;
        };
      });

  nixConfig = {
    extra-substituters = "https://devenv.cachix.org";
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    allow-unfree = true;
  };
}

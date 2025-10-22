{
  inputs = {
    nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, pre-commit-hooks, ... }@inputs:
    let forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = forEachSystem (system: {
        devenv-up = self.devShells.${system}.default.config.procfileScript;
      });

      checks = forEachSystem (system: {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # Haskell checks
            fourmolu.enable = true;
            hlint.enable = true;

            # Frontend checks
            prettier = {
              enable = true;
              types_or = [ "javascript" "typescript" "svelte" "json" "css" "html" ];
            };

            # General checks
            trailing-whitespace = {
              enable = true;
              types = [ "text" ];
            };
            end-of-file-fixer = {
              enable = true;
              types = [ "text" ];
            };

            # Format checks
            nixpkgs-fmt.enable = true;
          };
        };
      });

      devShells = forEachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pre-commit-check = self.checks.${system}.pre-commit-check;
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [{
              # https://devenv.sh/reference/options/
              packages = with pkgs; [
                # Haskell tools
                haskellPackages.haskell-language-server
                haskellPackages.fourmolu
                haskellPackages.hlint

                # Node.js for frontend
                nodejs_22

                # Formatting tools
                nixpkgs-fmt
                nodePackages.prettier

                # Python for pre-commit hooks
                python3Packages.pre-commit-hooks
              ];

              languages.haskell.enable = true;

              # Git hooks
              pre-commit.hooks = {
                fourmolu.enable = true;
                hlint.enable = true;
                nixpkgs-fmt.enable = true;
                trailing-whitespace.enable = true;
                end-of-file-fixer.enable = true;
              };

              # Shell hook to install git hooks
              enterShell = ''
                ${pre-commit-check.shellHook}
                echo "Git hooks installed!"
                echo "Run 'stack build' to build the Haskell project"
                echo "Run 'cd frontend && npm install && npm run dev' to start the frontend"
              '';
            }];
          };
        });
    };
}

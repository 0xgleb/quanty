{ pkgs, ... }:
{
  # Git hooks configuration for the quanty project
  # This uses the pre-commit-hooks.nix framework

  hooks = {
    # Haskell checks
    fourmolu = {
      enable = true;
      name = "fourmolu";
      description = "Format Haskell code with fourmolu";
      entry = "${pkgs.haskellPackages.fourmolu}/bin/fourmolu --mode inplace --check-idempotence";
      files = "\\.(hs|lhs)$";
      language = "system";
    };

    hlint = {
      enable = true;
      name = "hlint";
      description = "Lint Haskell code with hlint";
      entry = "${pkgs.haskellPackages.hlint}/bin/hlint";
      files = "\\.(hs|lhs)$";
      language = "system";
    };

    # Frontend checks
    prettier = {
      enable = true;
      name = "prettier";
      description = "Format frontend code with prettier";
      entry = "cd frontend && ${pkgs.nodePackages.prettier}/bin/prettier --write";
      files = "\\.(js|ts|svelte|json|css|html)$";
      language = "system";
    };

    # General checks
    trailing-whitespace = {
      enable = true;
      name = "trailing-whitespace";
      description = "Remove trailing whitespace";
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/trailing-whitespace-fixer";
      types = [ "text" ];
    };

    end-of-file-fixer = {
      enable = true;
      name = "end-of-file-fixer";
      description = "Ensure files end with a newline";
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/end-of-file-fixer";
      types = [ "text" ];
    };

    check-yaml = {
      enable = true;
      name = "check-yaml";
      description = "Validate YAML files";
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/check-yaml";
      files = "\\.ya?ml$";
    };

    check-json = {
      enable = true;
      name = "check-json";
      description = "Validate JSON files";
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/check-json";
      files = "\\.json$";
    };

    # Nix checks
    nixpkgs-fmt = {
      enable = true;
      name = "nixpkgs-fmt";
      description = "Format Nix code";
      entry = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
      files = "\\.nix$";
    };
  };
}

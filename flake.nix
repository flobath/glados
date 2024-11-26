{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/24.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        config,
        ...
      }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc925;

          devShell = {
            # Disable the automatically generated devShell to
            # create a separate devShell with additional packages
            enable = false;

            tools = hp: {
              cabal-install = hp.cabal-install;
              haskell-language-server = hp.haskell-language-server;
              ghcid = hp.ghcid;
              hlint = hp.hlint;
            };

            hlsCheck.enable = true;
          };
        };

        # Custom devShell with additional packages
        devShells.default = pkgs.mkShell {
          name = "Superb haskell flake shell";
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          nativeBuildInputs = with pkgs; [bashInteractiveFHS chez];
        };

        packages.default = self'.packages.GLaDOS;
      };
    };
}

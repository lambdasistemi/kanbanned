{
  description = "Terminal kanban board for agent-daemon";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    let version = self.dirtyShortRev or self.shortRev;
    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
        };
        project = import ./nix/project.nix {
          indexState = "2025-12-07T00:00:00Z";
          inherit pkgs;
        };
      in {
        packages = project.packages // {
          default = project.packages.kanbanned;
        };
        inherit (project) devShells;
      });
}

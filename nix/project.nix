{ indexState, pkgs, ... }:

let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.pkg-config
      pkgs.libvterm-neovim
    ];
  };

  mkProject = { lib, pkgs, ... }: {
    name = "kanbanned";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [{
      packages.kanbanned.flags.werror = true;
      packages.kanbanned.components.library = {
        libs = pkgs.lib.mkForce [ pkgs.libvterm-neovim ];
        build-tools = [ pkgs.pkg-config ];
        configureFlags = [
          "--extra-lib-dirs=${pkgs.libvterm-neovim}/lib"
          "--extra-include-dirs=${pkgs.libvterm-neovim}/include"
        ];
      };
      packages.kanbanned.components.tests.unit-tests = {
        libs = pkgs.lib.mkForce [ pkgs.libvterm-neovim ];
        build-tools = [ pkgs.pkg-config ];
        configureFlags = [
          "--extra-lib-dirs=${pkgs.libvterm-neovim}/lib"
          "--extra-include-dirs=${pkgs.libvterm-neovim}/include"
        ];
      };
    }];
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.kanbanned =
    project.hsPkgs.kanbanned.components.exes.kanbanned;
  packages.unit-tests =
    project.hsPkgs.kanbanned.components.tests.unit-tests;
}

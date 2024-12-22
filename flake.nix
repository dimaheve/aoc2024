{
  description = "Haskell Development Environment for Advent of Code (AoC)";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs.haskellPackages; [
              ghc
              cabal-install
              haskell-language-server
              hlint               
              ormolu              
              ghcid               
              parallel
              split
              hoogle              
              text_2_1_1
              containers_0_7
              deepseq_1_5_1_0
              vector
              split
              array_0_5_8_0
              megaparsec_9_6_1
              mtl_2_3_1
              PSQueue
              search-algorithms
              fgl
              unordered-containers
              hashable
              algebraic-graphs
              linear
            ] ++ (with pkgs; [
              niv                 
              entr                
              gnumake
            ]);

            shellHook = ''
              export LANG=en_US.UTF-8
              export LC_ALL=en_US.UTF-8
              export PATH=$HOME/.cabal/bin:$PATH
              echo "Welcome to your Haskell development environment for Advent of Code!"
            '';
          };
        };
      }
    );
}

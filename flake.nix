{
  description = "Haskell development with gloss support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskellPackages;

        myDevTools = [
          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.fourmolu
          hPkgs.hlint
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.implicit-hie
          hPkgs.cabal-install
          hPkgs.gloss
          stack-wrapped
          
          pkgs.libGL
          pkgs.libGLU
          pkgs.freeglut
          pkgs.openal
          pkgs.freealut
        ];

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

      in {
        # This builds the package using Nix (will work if dependencies are in nixpkgs)
        packages.default = pkgs.haskellPackages.callCabal2nix "glossy-ball-demo" ./. {};

        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;
          LD_LIBRARY_PATH = "${pkgs.freeglut}/lib";
          shellHook = ''
            echo "Haskell development environment ready!"
            echo "GHC: $(which ghc)"
            echo "Cabal: $(which cabal)"
            echo ""
            echo "For cabal builds, cabal will download dependencies from Hackage."
            echo "For Nix builds, use: nix build"
          '';
        };
      });
}
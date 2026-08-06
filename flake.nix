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
          stack-wrapped
          
          pkgs.libGL
          pkgs.libGLU
          pkgs.freeglut
          pkgs.openal
          pkgs.freealut
          
          hPkgs.gloss
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

        # Base package
        basePackage = hPkgs.callCabal2nix "glossy-ball-demo" ./. {};

      in rec {
        packages.default = basePackage.overrideAttrs (old: {
          # Use preInstall instead of postInstall to ensure directories exist
          preInstall = (old.preInstall or "") + ''
            echo "Creating data directory structure..."
            mkdir -p "$out/share/glossy-ball-demo"
          '';
          
          # Copy data files after cabal installs the binary
          postInstall = (old.postInstall or "") + ''
            echo "=== Installing data files to $out ==="
            
            # Ensure the data directory exists
            mkdir -p "$out/share/glossy-ball-demo"
            
            # Copy each data directory from source
            for d in sounds assets config data resources; do
              if [ -d "$src/$d" ]; then
                echo "Copying $d/ to $out/share/glossy-ball-demo/"
                cp -r "$src/$d" "$out/share/glossy-ball-demo/"
              fi
            done
            
            # Copy loose data files from root
            cd "$src"
            for ext in wav mp3 png jpg json txt; do
              for f in *."$ext"; do
                if [ -f "$f" ]; then
                  echo "Copying $f"
                  cp "$f" "$out/share/glossy-ball-demo/"
                fi
              done
            done
            
            # Show what was installed
            echo "Data files installed in $out/share/glossy-ball-demo:"
            find "$out/share/glossy-ball-demo" -type f | sort || echo "No data files found"
          '';
        });

        apps = {
          default = {
            type = "app";
            program = "${packages.default}/bin/glossyBallBounce";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;
          LD_LIBRARY_PATH = "${pkgs.freeglut}/lib";
          shellHook = ''
            echo "Haskell development environment ready!"
            echo "GHC: $(which ghc)"
            echo "Cabal: $(which cabal)"
          '';
        };
      });
}
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        name = "AoC2022";
        t = lib.trivial;
        hl = haskell.lib;
        hp = haskellPackages;

        aoc-script = writeShellScriptBin "aoc" ''
          me=$(basename $0)
          usage() {
              echo -e "Usage:\n" \
                  "  $me solve [DAY]\n" \
                  "  $me test [DAY]\n" \
                  "  $me bench [DAY]\n" \
                  "  $me hoogle" >&2
          }

          if [ -z "$1" ]; then
              usage
          fi

          case "$1" in

              "solve")
                  if [ -z "$2" ]; then
                      cabal run AoC
                  else
                      cabal run AoC -- "$2"
                  fi
                  ;;

              "test")
                  if [ -n "$2" ]; then
                      test="-p $2"
                  fi
                  find . -name "*.hs" | ${entr}/bin/entr -sc \
                                  "cabal test --test-options=\"$test --color always\"\
                                              --test-show-details streaming\
                                              --verbose=0"
                  ;;

              "bench")

                  if [ -n "$2" ]; then
                      bench="--benchmark-options=$2"
                  fi
                  find . -name "*.hs" | ${entr}/bin/entr -sc "cabal bench $bench"
                  ;;

              "hoogle")
                  hoogle server --local
                  ;;

          esac
        '';


        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in
          hp.developPackage {
            inherit name;
            root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            returnShellEnv = devTools != [ ];

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in
      {
        packages.pkg = project [ ];

        defaultPackage = self.packages.${system}.pkg;

        devShell = project (with hp; [
          aoc-script
          cabal-fmt
          cabal-install
          haskell-language-server
        ]);
      });
}

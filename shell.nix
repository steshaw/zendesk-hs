with (import <nixpkgs> {});

let
  ghc = nur.repos.mpickering.ghc.ghc802;
in
builtins.trace "ghc.version = ${ghc.version}" haskell.lib.buildStackProject {
  inherit ghc;
  name = "env";
  buildInputs = [
    haskellPackages.happy
    zlib
  ];
}

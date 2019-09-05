with (import <nixpkgs> {});
{ ghc ? haskell.compiler.ghc844 }:

builtins.trace "ghc.version = ${ghc.version}" haskell.lib.buildStackProject {
  inherit ghc;
  name = "env";
  buildInputs = [
    haskellPackages.happy
    zlib
  ];
  shellHook = ''
    source .config/environment.secret.sh
  '';
}

with (import <nixpkgs> {});
{ ghc ? haskell.compiler.ghc865 }:

(if true then (a: b: b) else builtins.trace) "ghc.version = ${ghc.version}" haskell.lib.buildStackProject {
  inherit ghc;
  name = "env";
  buildInputs = [
    haskellPackages.happy

    gmp
    libffi
    ncurses
    zlib
  ];
  shellHook = ''
    source .config/environment.secret.sh
  '';
}

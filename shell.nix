#
#  Development Environment Sandbox
#

with import ./nix/env.nix; with nixpkgs; mkShell {
  inherit src GIT_SSL_CAINFO LANG LC_ALL PAGER shellHook;
  buildInputs = buildInputs ++ [
    wget gnupg coreutils procps-ng
    which less curl ripgrep tree
    openssh man-db
  ] ++ (with haskellPackages; [
    cabal-install stack ghci ghcid
    ghcide hasktags haskell-language-server
    fourmolu hoogle hlint ]); }

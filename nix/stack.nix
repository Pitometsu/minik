#
#  Build Environment Sandbox
#

with import ./env.nix; nixpkgs.mkShell {
  inherit src buildInputs GIT_SSL_CAINFO LANG LC_ALL PAGER shellHook;
  STACK_PLATFORM_VARIANT = "nix";
  STACK_IN_NIX_SHELL = 1;
  STACK_IN_NIX_EXTRA_ARGS = "";
  # LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath nativeBuildInputs;
  NIX_PATH = builtins.getEnv "NIX_PATH";
  NIX_PROFILES = builtins.getEnv "NIX_PROFILES";
  NIX_SSL_CERT_FILE = builtins.getEnv "NIX_SSL_CERT_FILE"; }

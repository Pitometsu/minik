# TODO: use https://www.tweag.io/blog/2020-05-25-flakes/
#
#  To update channel lock:
#
# nix-shell --pure -Q -p nix gitMinimal nix-prefetch-git cacert --run \
#  'nix-prefetch-git --quiet https://github.com/NixOS/nixpkgs "refs/heads/nixos-unstable"' > \
#   ./nix/channel/lock

let
  channel = with builtins; fromJSON
    (readFile ./lock);
in fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/" +
        "${channel.rev}.tar.gz";
  inherit (channel) sha256;
}

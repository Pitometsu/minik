let
  channel = import ./channel;
  nixpkgs = import (
    builtins.trace "Use channel: ${channel}" channel
  ) {}; in
with nixpkgs; rec {
  inherit nixpkgs;
  src = builtins.toString ../.;
  haskellPackages = haskell.packages.ghc8107;
  buildInputs = with haskellPackages; [
    gitMinimal ghc
  ];
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  LANG = "C.UTF-8";
  LC_ALL = "C.UTF-8";
  PAGER="less -R";
  shellHook = ''
    ulimit -Sv 8388608
    ulimit -v 10485760
  '';
}

  # let
  #   fromYAML = yaml: builtins.fromJSON (
  #     builtins.readFile (
  #       pkgs.runCommand "from-yaml"
  #         {
  #           inherit yaml;
  #           allowSubstitutes = false;
  #           preferLocalBuild = true;
  #         }
  #         ''
  #           ${pkgs.remarshal}/bin/remarshal  \
  #             -if yaml \
  #             -i <(echo "$yaml") \
  #             -of json \
  #             -o $out
  #         ''
  #     )
  #   );

  #   readYAML = path: fromYAML (builtins.readFile path);
  # in

  # let
  #   stackage = import (fetchTarball {
  #     url = "https://stackage.serokell.io/ad0kwmbwynr9hk0g2xl9jc0cxnhjvl2f-stackage/default.nix.tar.gz";
  #     sha256 = "1imf2h1brpgpl5rfbyr7iqh3xpqflcgdi7p6g0nzx022yyrg0m91";
  #   });
  #   stackPackages = stackage."lts-18.18";
  # in

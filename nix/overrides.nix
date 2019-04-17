{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  wa = (
    with rec {
      waSource = pkgs.lib.cleanSource ../.;
      waBasic  = self.callCabal2nix "wa" waSource { };
    };
    overrideCabal waBasic (old: {
    })
  );
}

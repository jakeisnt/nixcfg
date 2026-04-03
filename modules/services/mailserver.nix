{ lib, config, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.mailserver;
in {
  options.modules.services.mailserver = { enable = mkBoolOpt false; };
  # simple-nixos-mailserver has been removed; this module is a stub.
}

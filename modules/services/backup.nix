{ config, lib, pkgs, ... }:

# This service backs up to the cloud and to a physical device, somehow.
with lib;
with lib.my;
let
  cfg = config.modules.services.backup;
  domain = config.networking.domain;
in {
  options.modules.services.backup = {
    enable = mkBoolOpt false;
    registration = mkBoolOpt false;
    mail = mkBoolOpt false;
    yubikey = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    users.users.duplicati.isNormalUser = true;
    services.duplicati = {
      enable = true;
      user = "jake";
    };
  };
}

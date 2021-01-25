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
    services.duplicati = {
      enable = true;
      user = "jake";
    };

    services.duplicity = {
      enable = true;
      include = [ XDG_PICTURES_DIR XDG_WORK_DIR ];
      targetUrl = "# b2://account_id[:application_key]@bucket_name/[folder/]";
      frequency = "daily";
      secretFile = "${XDG_CONFIG_DIR}/duplicity/secretFile";
    };

    home.configFile = {
      "duplicity/secretFile".text = ''
        PASSPHRASE=
        AWS_SECRET_KEY_ID=
        AWS_SECRET_ACCESS_KEY=
        B2_ACCOUNT="AAA"
        B2_KEY="BBB"
        B2_BUCKET="CCC"
        B2_DIR="backups"
        ENC_KEY="EEEEEEEE"
        SGN_KEY="FFFFFFFF"
        PASSPHRASE="GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG"
        SIGN_PASSPHRASE="HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
      '';

    };
  };
}

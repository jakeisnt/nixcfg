{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gnome;
  colors = config.modules.theme.color;
in {
  options.modules.desktop.gnome = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    security.pam.services.gdm.enableGnomeKeyring = true;
    # security.pam.services = with pkgs.gnome3; [{
    #   name = "gnome_keyring";
    #   text = ''
    #     auth optional ${gnome_keyring}/lib/security/pam_gnome_keyring.so
    #     session optional ${gnome_keyring}/lib/security/pam_gnome_keyring.so auto_start
    #     password optional ${gnome_keyring}/lib/security/pam_gnome_keyring.so
    #   '';
    # }];
    services = {
      gnome3.gnome-keyring.enable = true;
      xserver = {
        enable = true;
        xkbOptions = "caps:swapescape";
        displayManager.gdm.enable = true;
        desktopManager.gnome3.enable = true;

        # todo: device-specific
        # monitorSection = ''
        #   DisplaySize 508 285
        # '';
      };
    };
  };
}

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # DEPRECATED: Can be removed!
    # sound.enable = true;
    xdg.portal = mkIf config.modules.desktop.sway.enable {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };

    # control audio utilities
    user.extraGroups = [
      "audio"
    ];

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
    };

    user.packages = with pkgs; [
      # dynamically linked to; supports camera connection
      libcamera
    ];
  };
}

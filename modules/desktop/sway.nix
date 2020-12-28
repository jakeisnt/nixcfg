{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
    ];

    services = {
      wayland.windowManager.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
      };
    };

    home.packages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako
      alacritty
      dmenu
      wl-clipboard
    ];
  };
}

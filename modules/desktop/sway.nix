{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # wayland.windowManager.sway = {
    #   enable = true;
    #   wrapperFeatures.gtk = true;
    # };
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    environment.systemPackages = with pkgs; [
      lightdm
      dunst
      swaylock
      swayidle
      wl-clipboard
      mako
      alacritty
      dmenu
      wl-clipboard
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
    ];
  };
}

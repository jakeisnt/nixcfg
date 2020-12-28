{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    environment.systemPackages = with pkgs; [
      lightdm
      swaylock
      swayidle
      wl-clipboard
      mako
      alacritty
      dmenu
      wl-clipboard
      libnotify
    ];
  };
}

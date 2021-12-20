{ config, lib, pkgs, ... }:

# general settings for all wayland compositors

with lib;
with lib.my;
let cfg = config.modules.wayland;
in {

  options.modules.wayland = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    env.XDG_SESSION_TYPE = "wayland";

    modules.wayland.wofi.enable = true;

    user.extraGroups = [
      # permission to control video utilities
      # needed for 'light' etc
      "video"
    ];

    user.packages = with pkgs; [
      xwayland
      qt5.qtwayland
      wl-clipboard
      sway-contrib.grimshot
      wf-recorder
      # due to overlay these are now wayland clipboard interoperable
      xclip
      xsel

      # adjust screen brightness
      light
    ];

  };
}

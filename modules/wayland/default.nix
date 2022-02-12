# Nice-to-haves for all Wayland compositors.

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.wayland;
in {

  options.modules.wayland = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    env.XDG_SESSION_TYPE = "wayland";

    modules.wayland.wofi.enable = true;

    user.extraGroups = [
      # control video utilities
      "video"
    ];

    programs.light.enable = true;

    user.packages = with pkgs; [
      xwayland
      qt5.qtwayland
      wl-clipboard
      sway-contrib.grimshot
      wf-recorder
      # due to overlay these are now wayland clipboard interoperable
      xclip
      xsel
      # versatile audio control
      playerctl
    ];
  };
}

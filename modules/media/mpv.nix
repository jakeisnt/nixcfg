{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.media.mpv;
in {
  options.modules.media.mpv = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # image viewer
      imv
      mpv-with-scripts
      mpvc # CLI controller for mpv
      (mkIf config.programs.sway.enable celluloid) # nice GTK GUI for mpv
    ];
  };
}

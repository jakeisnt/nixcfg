{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.lf;
in {
  options.modules.shell.lf = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [
      pkgs.lf
      pkgs.w3m # w3m image preview
      pkgs.poppler_utils # pdf preview
      pkgs.ffmpegthumbnailer # video thumbnails
    ];

    home.configFile = {
    };
  };
}

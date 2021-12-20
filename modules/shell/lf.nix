# fast terminal browsing
{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.lf;
in {
  options.modules.shell.lf = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      lf # file browser
      w3m # w3m image preview
      poppler_utils # pdf preview
      ffmpegthumbnailer # video thumbnails
      xdg_utils # xdg-open is used a lot by lf
    ];

    home.configFile = {
      "lf" = {
        source = "${configDir}/lf";
        recursive = true; # include all of the utils for lf too
      };
    };
  };
}

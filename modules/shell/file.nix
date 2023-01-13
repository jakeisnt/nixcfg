# fast terminal browsing
{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.file;
in {
  options.modules.shell.file = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      joshuto
      poppler_utils # pdf preview
      ffmpegthumbnailer # video thumbnails
      xdg_utils # xdg-open is used a lot
    ];

    home.configFile = {
      "joshuto" = {
        source = "${configDir}/joshuto";
        recursive = true; # include all utils, config files, etc...
      };
    };
  };
}

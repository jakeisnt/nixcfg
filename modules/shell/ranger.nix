{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.ranger;
in {
  options.modules.shell.ranger = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [
      pkgs.ranger
      pkgs.w3m # w3m image preview
      pkgs.poppler_utils # pdf preview
      pkgs.ffmpegthumbnailer # video thumbnails
    ];
  };
}

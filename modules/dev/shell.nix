# POSIX shell is a crime

{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.shell;
in {
  options.modules.dev.shell = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      shellcheck
    ];
  };
}

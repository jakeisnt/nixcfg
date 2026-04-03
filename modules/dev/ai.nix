# modules/dev/ai.nix --- AI coding assistants
#
# Claude Code (native binary via ryoppippi/nix-claude-code) and opencode.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ai;
in {
  options.modules.dev.ai = {
    enable = mkBoolOpt false;
    claude.enable = mkBoolOpt true;
    opencode.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (optionals cfg.claude.enable [ claude-code ])
      ++ (optionals cfg.opencode.enable [ unstable.opencode ]);
  };
}

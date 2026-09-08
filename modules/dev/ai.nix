# modules/dev/ai.nix --- AI coding assistants
#
# Bun, Codex, and opencode.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ai;
in {
  options.modules.dev.ai = {
    enable = mkBoolOpt false;
    codex.enable = mkBoolOpt true;
    opencode.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ bun ]
      ++ (optionals cfg.codex.enable [ unstable.codex ])
      ++ (optionals cfg.opencode.enable [ unstable.opencode ]);
  };
}

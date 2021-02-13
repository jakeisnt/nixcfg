{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
in {
  options.modules.editors = { default = mkOpt types.str "vim"; };

  config = {
    env.EDITOR = cfg.default;
    user.packages = with pkgs; [
      # use wakatime for all editors
      wakatime
      # some wakatime configurations are picky about python
      python
    ];

    environment.shellAliases = {
      wakatime =
        "${pkgs.wakatime}/bin/wakatime --key ${secrets.wakatime.apiKey}";
    };
  };
}

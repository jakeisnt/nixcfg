{ config, options, lib, pkgs,  ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
in {
  options.modules.editors = { default = mkOpt types.str "nvim"; };

  config = {
    env.EDITOR = cfg.default;
    user.packages = with pkgs; [
      # use wakatime for all editors
      wakatime
      # some wakatime configurations are picky about python availability
      python
    ];

    sops.secrets."wakatime_apikey" = {};

    environment.shellAliases = {
      # TODO restore wakatime
      # wakatime =
      #   "${pkgs.wakatime}/bin/wakatime --key \$(cat ${sops.secrets."wakatime_apikey".path})";
    };
  };
}

{ config, options, lib, pkgs,  ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
    secrets = lib.my.secrets;
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

    environment.shellAliases = {
      # TODO restore wakatime
      # secrets.wakatime.apiKey
      wakatime =
        "${pkgs.wakatime}/bin/wakatime-cli --key ${secrets.wakatime.apiKey}";
    };
  };
}

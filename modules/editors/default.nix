{ config, options, lib, pkgs,  ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
    secrets = lib.my.secrets;
in {
  options.modules.editors = { default = mkOpt types.str "emacsclient -c"; };

  config = {
    env.EDITOR = cfg.default;
    user.packages = with pkgs; [wakatime];

    environment.shellAliases = {
      wakatime-cli =
        "${pkgs.wakatime}/bin/wakatime-cli --key ${secrets.wakatime.apiKey}";
      wakatime =
        "${pkgs.wakatime}/bin/wakatime-cli --key ${secrets.wakatime.apiKey}";
    };
  };
}

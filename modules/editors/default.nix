{ config, options, lib, pkgs,  ... }:

with lib;
with lib.my;
let cfg = config.modules.editors;
in {
  options.modules.editors = { default = mkOpt types.str "emacsclient -c"; };

  config = {
    env.EDITOR = cfg.default;
    user.packages = with pkgs; [wakatime-cli];

    environment.shellAliases = {

      # TODO: Bring back the wakatime API key.
      # wakatime-cli =
      #   "${pkgs.wakatime}/bin/wakatime-cli --key <runtime-secret>";
      # wakatime =
      #   "${pkgs.wakatime}/bin/wakatime-cli --key <runtime-secret>";
    };
  };
}

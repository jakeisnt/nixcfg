{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.hardware.extraHosts;
  src = builtins.fetchTarball {
    url = "https://github.com/StevenBlack/hosts/archive/3.3.4.tar.gz";
    sha256 = "15474wjzwldfrbacvnwii7wjil83lh1wykq0h1sf59fq4fv75inj";
  };
  blockAllButSocial = "${src}/alternates/fakenews-gambling-porn/hosts";
  blockAll = "${src}/alternates/fakenews-gambling-porn-social/hosts";
in {
  options.modules.hardware.extraHosts = {
    enable = mkBoolOpt false;
    allowSocial = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    networking.extraHosts =
      readFile (if cfg.allowSocial then blockAllButSocial else blockAll);
  };
}

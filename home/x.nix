{ pkgs, ... }:
{
  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "arcticicestudio";
      repo = "nord-xresources";
      rev = "ad8d70435ee0abd49acc7562f6973462c74ee67d";
      sha256 = "1labc69iis5nx7cz88lkp0jsm5aq3ywih15g1fhc4814hv0x0ckf";
    } + "/src/nord"
  );
}

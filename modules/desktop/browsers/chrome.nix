{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.chrome;
in {
  options.modules.desktop.browsers.chrome = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      # TODO: change to something more secure than google
      defaultSearchProviderSearchURL =
        "https://encrypted.google.com/search?q={searchTerms}&{google:RLZ}{google:originalQueryForSuggestion}{google:assistedQueryStats}{google:searchFieldtrialParameter}{google:searchClient}{google:sourceId}{google:instantExtendedEnabledParameter}ie={inputEncoding}";
      defaultSearchProviderSuggestURL =
        "https://encrypted.google.com/complete/search?output=chrome&q={searchTerms}";
      homepageLocation = "https://github.com";
      extensions = [
        "nngceckbapebfimnlniiiahkandclblb" # bitwarden
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "kkkjlfejijcjgjllecmnejhogpbcigdc" # org-capture
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
      ];

      # add more options when using chromium !!
      # info found here: https://cloud.google.com/docs/chrome-enterprise/policies/#miscellaneous
      extraOpts = {
        "BrowserSignin" = 0;
        "PasswordManagerEnabled" = false;
        "SyncDisabled" = true;
        "CloudReportingEnabled" = false;
        "SafeBrowsingEnabled" = false;
        "ReportSafeBrowsingData" = false;
        "AllowDinosaurEasterEgg" = false; # : (
        "AllowOutdatedPlugins" = true;

        # voice assistant
        "VoiceInteractionContextEnabled" = false;
        "VoiceInteractionHotwordEnabled" = false;
        "VoiceInteractionQuickAnswersEnabled" = false;
      };
    };
  };
}

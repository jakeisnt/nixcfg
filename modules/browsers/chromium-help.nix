{ config, lib, pkgs, ... }:

# settings for all chromium browsers.

with lib;
with lib.my;
let cfg = config.modules.browsers.chromium-help;
in {
  options.modules.browsers.chromium-help = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
  # TODO: chrome with wayland?
  # chromium.commandLineArgs = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
  # enable wayland for chromium
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
        "ddehdnnhjimbggeeenghijehnpakijod" # remove scrollbar
        "ikdgincnppajmpmnhfheflannaiapmlm" # ampie: social interaction with webpages
        # custom css;
        # ::-webkit-scrollbar {
        #   display: none;
        # }
        # must be added to Rescroller extension
        # also sign into lastfm scrobbler
      ];

      # add more options when using chromium !!
      # info found here: https://cloud.google.com/docs/chrome-enterprise/policies/#miscellaneous
      extraOpts = {
        # "BrowserSignin" = 0;
        "PasswordManagerEnabled" = false;
        # "SyncDisabled" = true;
        "CloudReportingEnabled" = false;
        "SafeBrowsingEnabled" = false;
        "ReportSafeBrowsingData" = false;
        "AllowDinosaurEasterEgg" = false; # : (
        "AllowOutdatedPlugins" = true;
        "DefaultBrowserSettingEnabled" = false;

        # voice assistant
        "VoiceInteractionContextEnabled" = false;
        "VoiceInteractionHotwordEnabled" = false;
        "VoiceInteractionQuickAnswersEnabled" = false;
      };
    };
  };
}

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
  # TODO: Enable wayland for chromium. Currently this is a saved browser setting, but it might not carry over.
    programs.chromium = {
      enable = true;
      # TODO: change to something more secure than google
      defaultSearchProviderSearchURL =
        "https://encrypted.google.com/search?q={searchTerms}&{google:RLZ}{google:originalQueryForSuggestion}{google:assistedQueryStats}{google:searchFieldtrialParameter}{google:searchClient}{google:sourceId}{google:instantExtendedEnabledParameter}ie={inputEncoding}";
      defaultSearchProviderSuggestURL =
        "https://encrypted.google.com/complete/search?output=chrome&q={searchTerms}";
      homepageLocation = "https://github.com";
      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "kkkjlfejijcjgjllecmnejhogpbcigdc" # org-capture
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
        "ddehdnnhjimbggeeenghijehnpakijod" # remove scrollbar
        # "ikdgincnppajmpmnhfheflannaiapmlm" # ampie: social interaction with webpages
        # ^ cool, but it always gets in the way!
        # custom css;
        # ::-webkit-scrollbar {
        #   display: none;
        # }
        # must be added to Rescroller extension
        # also sign into lastfm scrobbler
      ];

      extraOpts = {
        # I like having my information synced across browser sessions,
        # and I'm okay with surrendering some information to Google to do so.
        # The other settings may not work without managed browser control, though.

        # "BrowserSignin" = 0;
        # "SyncDisabled" = true;

        # Definitely want to turn off these though.
        "PasswordManagerEnabled" = false;
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

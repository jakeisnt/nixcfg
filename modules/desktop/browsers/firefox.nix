{ options, res, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.firefox;
  # use a custom build of firefox
  # TODO: add anti tracking policies at build time
  # https://wiki.kairaven.de/open/app/firefox in german : (
  firefoxWrapped = pkgs.wrapFirefox pkgs.firefox-unwrapped {
    forceWayland = config.modules.desktop.sway.enable;
    extraPolicies = {
      CaptivePortal = false;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFirefoxAccounts = true;
      FirefoxHome = {
        Pocket = false;
        Snippets = false;
      };
      UserMessaging = {
        ExtensionRecommendations = false;
        SkipOnboarding = true;
      };
    };
    extraPrefs = ''
      // Show more ssl cert infos
      lockPref("security.identityblock.show_extended_validation", true);
    '';
  };
in {
  options.modules.desktop.browsers.firefox = with types; {
    enable = mkBoolOpt false;
    profileName = mkOpt types.str config.user.name;

    settings = mkOpt' (attrsOf (oneOf [ bool int str ])) { } ''
      Firefox preferences to set in <filename>user.js</filename>
    '';
    extraConfig = mkOpt' lines "" ''
      Extra lines to add to <filename>user.js</filename>
    '';

    userChrome = mkOpt' lines "" "CSS Styles for Firefox's interface";
    userContent = mkOpt' lines "" "Global CSS Styles for websites";
  };

  config = mkIf cfg.enable (mkMerge [{
    nixpkgs.overlays = [ inputs.nur.overlay ];
    user.packages = with pkgs; [
      firefoxWrapped
      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Open a private Firefox window";
        icon = "firefox";
        exec = "${firefox-bin}/bin/firefox --private-window";
        categories = "Network";
      })
    ];

    env.MOZ_ENABLE_WAYLAND = if config.modules.desktop.sway then "1" else "0";

    # find extensions here:
    # https://gitlab.com/rycee/nur-expressions/-/blob/master/pkgs/firefox-addons/generated-firefox-addons.nix
    # TODO request that redux-devtools is added:
    # https://addons.mozilla.org/en-US/firefox/addon/reduxdevtools/
    home-manager.users.jake.programs.firefox.extensions =
      with pkgs.nur.repos.rycee.firefox-addons;
      [
        bitwarden
        vimium
        buster-captcha-solver
        clearurls
        dark-night-mode
        decentraleyes
        org-capture
        ublock-origin
      ] ++ (if config.modules.dev.node.enable then [ react-devtools ] else [ ]);

    modules.desktop.browsers.firefox.settings = {
      "devtools.theme" = "dark";
      "widget.content.gtk-theme-override" = "Nordic";
      # Enable userContent.css and userChrome.css for our theme modules
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      # Don't use the built-in password manager; a nixos user is more likely
      # using an external one (you are using one, right?).
      "signon.rememberSignons" = false;
      # Do not check if Firefox is the default browser
      "browser.shell.checkDefaultBrowser" = false;
      # Disable the "new tab page" feature and show a blank tab instead
      # https://wiki.mozilla.org/Privacy/Reviews/New_Tab
      # https://support.mozilla.org/en-US/kb/new-tab-page-show-hide-and-customize-top-sites#w_how-do-i-turn-the-new-tab-page-off
      "browser.newtabpage.enabled" = false;
      "browser.newtab.url" = "about:blank";
      # Disable Activity Stream
      # https://wiki.mozilla.org/Firefox/Activity_Stream
      "browser.newtabpage.activity-stream.enabled" = false;
      # Disable new tab tile ads & preload
      # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
      # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
      # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
      # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
      # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
      "browser.newtabpage.enhanced" = false;
      "browser.newtab.preload" = false;
      "browser.newtabpage.directory.ping" = "";
      "browser.newtabpage.directory.source" = "data:text/plain,{}";
      # Disable some not so useful functionality.
      "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
      "extensions.htmlaboutaddons.recommendations.enabled" = false;
      "extensions.htmlaboutaddons.discover.enabled" = false;
      "extensions.pocket.enabled" = false;
      "app.normandy.enabled" = false;
      "app.normandy.api_url" = "";
      "extensions.shield-recipe-client.enabled" = false;
      "app.shield.optoutstudies.enabled" = false;
      # Disable battery API
      # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
      # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
      "dom.battery.enabled" = false;
      # Disable "beacon" asynchronous HTTP transfers (used for analytics)
      # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
      "beacon.enabled" = false;
      # Disable pinging URIs specified in HTML <a> ping= attributes
      # http://kb.mozillazine.org/Browser.send_pings
      "browser.send_pings" = false;
      # Disable gamepad API to prevent USB device enumeration
      # https://www.w3.org/TR/gamepad/
      # https://trac.torproject.org/projects/tor/ticket/13023
      "dom.gamepad.enabled" = false;
      # Don't try to guess domain names when entering an invalid domain name in URL bar
      # http://www-archive.mozilla.org/docs/end-user/domain-guessing.html
      "browser.fixup.alternate.enabled" = false;
      # Disable telemetry
      # https://wiki.mozilla.org/Platform/Features/Telemetry
      # https://wiki.mozilla.org/Privacy/Reviews/Telemetry
      # https://wiki.mozilla.org/Telemetry
      # https://www.mozilla.org/en-US/legal/privacy/firefox.html#telemetry
      # https://support.mozilla.org/t5/Firefox-crashes/Mozilla-Crash-Reporter/ta-p/1715
      # https://wiki.mozilla.org/Security/Reviews/Firefox6/ReviewNotes/telemetry
      # https://gecko.readthedocs.io/en/latest/browser/experiments/experiments/manifest.html
      # https://wiki.mozilla.org/Telemetry/Experiments
      # https://support.mozilla.org/en-US/questions/1197144
      # https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html#id1
      "toolkit.telemetry.enabled" = false;
      "toolkit.telemetry.unified" = false;
      "toolkit.telemetry.archive.enabled" = false;
      "experiments.supported" = false;
      "experiments.enabled" = false;
      "experiments.manifest.uri" = "";
      # Disable health reports (basically more telemetry)
      # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
      # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
      "datareporting.healthreport.uploadEnabled" = false;
      "datareporting.healthreport.service.enabled" = false;
      "datareporting.policy.dataSubmissionEnabled" = false;

      # skip homepage when starting
      "browser.search.firstRunSkipsHomepage" = true;
      # allow use of system gtk dark theme
      "widget.content.allow-gtk-dark-theme" = true;
    };

    # Use a stable profile name so we can target it in themes
    home.file = let cfgPath = ".mozilla/firefox";
    in {
      "${cfgPath}/profiles.ini".text = ''
        [Profile0]
        Name=default
        IsRelative=1
        Path=${cfg.profileName}.default
        Default=1

        [General]
        StartWithLastProfile=1
        Version=2
      '';

      "${cfgPath}/${cfg.profileName}.default/user.js" =
        mkIf (cfg.settings != { } || cfg.extraConfig != "") {
          text = ''
            ${concatStrings (mapAttrsToList (name: value: ''
              user_pref("${name}", ${builtins.toJSON value});
            '') cfg.settings)}
            ${cfg.extraConfig}
          '';
        };

      "${cfgPath}/${cfg.profileName}.default/chrome/userChrome.css" =
        mkIf (cfg.userChrome != "") { text = cfg.userChrome; };

      "${cfgPath}/${cfg.profileName}.default/chrome/userContent.css" =
        mkIf (cfg.userContent != "") { text = cfg.userContent; };
    };
  }]);
}

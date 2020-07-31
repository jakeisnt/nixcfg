{ pkgs, ... }: {
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball
      "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
  };

  home.sessionVariables.BROWSER = "firefox";
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons;
      [
        # lastpass-password-manager
        # violentmonkey
        # torswitch
        # ublock-origin
        org-capture
      ];

    profiles = {
      jake = {
        isDefault = true;
        settings = {
          "devtools.theme" = "dark";
          "browser.search.isUS" = true;
          "browser.startup.firstrunSkipsHomepage" = true;
          "browser.startup.homepage" = "about:blank";
          "browser.search.region" = "US";
          "services.sync.prefs.sync.browser.startup.homepage" = false;
          "startup.homepage_welcome_url" = "about:blank";
          "browser.bookmarks.showMobileBookmarks" = true;
          "widget.content.allow-gtk-dark-theme" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.newtabpage.activity-stream.discoverystream.enabled" = false;
          "browser.newtabpage.activity-stream.feeds.aboutpreferences" = false;
          "browser.newtabpage.activity-stream.feeds.asrouterfeed" = false;
          "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" =
            false;
          "browser.newtabpage.activity-stream.feeds.favicon" = false;
          "browser.newtabpage.activity-stream.feeds.newtabinit" = false;
          "browser.newtabpage.activity-stream.feeds.places" = false;
          "browser.newtabpage.activity-stream.feeds.prefs" = false;
          "browser.newtabpage.activity-stream.feeds.recommendationproviderswitcher" =
            false;
          "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.feeds.sections" = false;
          "browser.newtabpage.activity-stream.feeds.snippets" = false;
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;

          "browser.link.open_newwindow" = 0; # always open tab in new window
          "browser.link.open_newwindow.restriction" = 2;
          "browser.link.open_newwindow.override.external" = 2;
        };

        userContent = ''
          /* Removes white loading page */
          @-moz-document url(about:blank), url(about:newtab), url(about:home) {
              html:not(#ublock0-epicker), html:not(#ublock0-epicker) body, #newtab-customize-overlay {
                  background: #2E3440 !important;
                  body{background-color: #2E3440 !important;}
              }
          }

          /* Hide scrollbar */
          :root{
              scrollbar-width: none !important;
          }
          @-moz-document url(about:privatebrowsing) {
              :root{
                  scrollbar-width: none !important;
              }
          }
        '';

        userChrome = ''
          html#main-window {
              --urlbar-text-font: "mononoki Nerd Font", sans-serif;
              --color-bg: #2E3440;
              --toolbar-bgcolor: var(--color-bg) !important;
              /* Firefox variables */
              --lwt-toolbar-field-focus-color: var(--color-bg) !important;
              --lwt-toolbar-field-color: #fefefa !important;
              --lwt-toolbar-field-background-color: var(--color-bg) !important;

              --arrowpanel-background: var(--color-bg) !important;
              --arrowpanel-color: var(--color-bg) !important;
              --arrowpanel-border-color: transparent !important;
          }

           /* attempts to make the top border transparent */
           .urlbarView-body-inner{ border-top-color: transparent !important }
           :root {--in-content-page-background: var(--color-bg); /*tab flash on new tab*/}

           /*changes "flash" on tab open color*/
           #browser vbox#appcontent tabbrowser, #content, #tabbrowser-tabpanels,
           browser[type=content-primary], browser[type=content] > html {
               background: var(--color-bg) !important;
           }

           body {
               background-color: var(--color-bg);
           }

           /* Remove border under navbar */
           #navigator-toolbox::after {
             border-bottom: 0px !important;
           }

           /* Remove border above navbar */
           #navigator-toolbox::after {
             border-top: 0px !important;
           }

           /* Blank the placeholder text in the search bar */
           #searchbar .textbox-input::placeholder {
             opacity: 0 !important;
           }

           #urlbar *|input::placeholder {
             /* opacity: 0 !important;  */
             text-align: center !important;
           }

           .urlbar-input-box{
             text-align: center !important;
           }

           /* Custom back and forward buttons */
           #back-button {
               border: none !important;
               color: var(--color-bg) !important;
               background-color: var(--color-bg) !important;
           }

           #forward-button {
               border: none !important;
               color: var(--color-bg) !important;
           }

           .panel-arrowbox {
               margin: 0 !important;
               display: none !important;
           }

           .panel-arrowcontent {
               opacity: 1 !important;
               margin: 0 !important;
           }

           /* Url bar suggestions list container */
           .urlbarView {
               background-color: var(--color-bg) !important;
               border-radius: 0 8px 8px !important;
               box-shadow: 0px 0px 40px rgba(0, 0, 0, 35%) !important;
               box-shadow: none !important;
               margin: auto !important;
               width: var(--urlbar-list-width) !important;
           }

           #TabsToolbar{
             visibility: collapse !important;
             display: none;
             background-color: var(--color-bg) !important;
           }

           #main-window:not([customizing]) #navigator-toolbox:not(:focus-within):not(:hover) {
             margin-top: -40px;
           }

           #navigator-toolbox {
             transition: 0.0s margin-top ease-out;
           }
        '';
      };
    };
  };
}

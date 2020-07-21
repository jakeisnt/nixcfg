{ pkgs, ...}:
{
    nixpkgs.config.packageOverrides = pkgs: {
        nur = import
            (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
                inherit pkgs;
            };
    };

    programs.firefox = {
        enable = true;
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            # lastpass-password-manager
            # violentmonkey
            # torswitch
            # ublock-origin
            org-capture
        ];

        profiles = {
            jake = {

                isDefault = true;
settings = {"browser.search.isUS" = true;
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
                    "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;
                    "browser.newtabpage.activity-stream.feeds.favicon" = false;
                    "browser.newtabpage.activity-stream.feeds.newtabinit" = false;
                    "browser.newtabpage.activity-stream.feeds.places" = false;
                    "browser.newtabpage.activity-stream.feeds.prefs" = false;
                    "browser.newtabpage.activity-stream.feeds.recommendationproviderswitcher" = false;
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
                            background: #282c34 !important;
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
                :root{ --uc-toolbar-height: 32px; }

                :root:not([uidensity="compact"]){--uc-toolbar-height: 38px}

                #TabsToolbar{ visibility: collapse !important; display: none; }

                /* moves tab bar below toolbar */
                :root:not([inFullscreen]) #nav-bar{
                    margin-top: calc(0px - var(--uc-toolbar-height));
                }

                #toolbar-menubar{
                    min-height:unset !important;
                    height:var(--uc-toolbar-height) !important;
                    position: relative;
                }

                #toolbar-menubar:not([inactive]){ z-index: 2 }
                #toolbar-menubar[inactive] > #menubar-items {
                    opacity: 0;
                    pointer-events: none;
                    margin-left: var(--uc-window-drag-space-width,0px)
                }

                html#main-window {
                    --color-bg: #21242b;
                    --toolbar-bgcolor: var(--color-bg) !important;

                    --tab-font: "Cantarell", sans-serif;
                    --tab-height: 0 !important;
                    --tabs-container-height: 0 !important;

                    --urlbar-container-margin: 5px;
                    --urlbar-list-width: 60%;
                    --urlbar-text-font: "mononoki Nerd Font", sans-serif;
                    --urlbar-text-size: 10pt;
                    --urlbar-text-weight: 700;

                    /* Firefox variables */
                    --lwt-toolbar-field-focus-color: #bbc2cf !important;
                    --lwt-toolbar-field-color: #fefefa !important;
                    --lwt-toolbar-field-background-color: var(--color-bg) !important;

                    --arrowpanel-background: #21242b !important;
                    --arrowpanel-color: #bbc2cf !important;
                    --arrowpanel-border-color: transparent !important;
                }

                /* Toolbar Elements */
                .browser-toolbar {
                    padding-left: 10px !important;
                    padding-top: 2px !important;
                    padding-right: 10px !important;
                }

                /* Bookmarks bar tweaks */
                #PlacesToolbar {
                    align-items: center !important;
                    display: flex !important;
                    font-family: "mononoki Nerd Font", sans-serif !important;
                    font-size: 8pt !important;
                    font-weight: 700 !important;
                    justify-content: center !important;
                }

                /* Custom back and forward buttons */
                #back-button {
                    border: none !important;
                    color: var(--color-bg) !important;
                }

                #forward-button {
                    border: none !important;
                    color: var(--color-bg) !important;
                }

                #navigator-toolbox {
                    --tabs-border-color: var(--color-bg) !important;
                    background-color: var(--color-bg) !important;
                    border: none !important;
                    box-shadow: none !important;
                    max-height: 70px  !important;
                    min-height: 70px !important;
                }

                /* Urlbar */
                ::-moz-selection {
                  background-color: #21242b !important;
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

                /* Url bar suggestions' selected row */
                .urlbarView:not(.megabar) .urlbarView-row:not([type="tip"])[selected],
                .urlbarView.megabar .urlbarView-row:not([type="tip"])[selected] > .urlbarView-row-inner {
                    background: #f9ff99 !important;
                    color: white !important;
                    fill-opacity: 1;
                }

                /* The text that says 'Search with ...' */
                .urlbarView-action {
                    color: white !important;
                }

                /* Url bar suggestions' row that is being hovered over */
                .urlbarView-row:hover {
                    background-color: none !important;
                    background-image: linear-gradient(77deg, #ffc386, #ff8989) !important;
                    color: white !important;
                }

                /* Text that says: This time, search with: */
                #urlbar-one-offs-header-label {
                    font-family: "Cantarell", sans-serif !important;
                    font-size: 14pt !important;
                    font-weight: 700 !important;
                    color: white !important;
                }

                #urlbar-background,
                #urlbar-container,
                .urlbar-input-box {
                    background-color: var(--color-bg) !important;
                    background-image: none !important;
                }

                #urlbar:not(.megabar),
                #urlbar.megabar > #urlbar-background,
                #searchbar {
                    border: none !important;
                    box-shadow: none !important;
                }

                /* Hacky way to make the url input bar centered */
                input#urlbar-input {
                    caret-color: transparent !important;
                    font-family: var(--urlbar-text-font) !important;
                    font-size: var(--urlbar-text-size) !important;
                    font-weight: var(--urlbar-text-weight) !important;
                    text-align: center !important;
                    width: 99999px !important;
                }

                .urlbarView-favicon,
                .urlbar-history-dropmarker,
                #star-button,
                #identity-box {
                    display: none !important;
                }

                #tracking-protection-icon-container,
                #urlbar-search-button {
                    background-color: none !important;
                    background-image: none !important;
                    border: none !important;
                }

                /* Browser tabs */
                /* Hide some bloat */
                .tab-close-button,
                .tab-line,
                .tabbrowser-tab::before,
                .tabbrowser-tab::after,
                .tabbrowser-tab:hover, 
                .tabbrowser-tab,
                .tab-content,
                .tab-stack,
                .tabbrowser-arrowscrollbox,
                .tabbrowser-tab .tab-icon-image {
                    display: none !important;
                    max-height: var(--tab-height) !important;
                    min-height: var(--tab-height) !important;
                }

                /* Settings menu pop-up */
                menupopup {
                    -moz-appearance: none !important;
                    background-color: #4a4a4f !important;
                    border: 1px solid #333 !important;
                    margin: -1px 0 0 0 !important;
                    border-radius: 4px !important;
                    transform: scale(.9, .9) !important;
                }

                menuitem:hover, menugroup:hover, menu:hover {
                    -moz-appearance: none !important;
                    background: #5c5c61 !important;
                }

                menuseparator {
                    -moz-appearance: none !important;
                    max-height: 1px !important;
                    border: none !important;
                    padding: 0.5px 0 !important;
                    background-color: #5c5c61 !important;
                }

                menuitem, menu {
                    -moz-appearance: none !important;
                    color: white !important;
                    padding: 1px 1px 2px 11px !important;
                    min-height: 25px !important;
                }

                menugroup {
                    padding: 2px !important;
                    background-color: transparent !important;
                }

                .menu-right {
                    -moz-appearance: none !important;
                    padding: 7px !important;
                    margin-right: 6px !important;
                    color: white !important;
                    border: solid white;
                    border-width: 0 2px 2px 0;
                    transform: rotate(-45deg) scale(.35);
                }

                .panel-arrowbox {
                    margin: 0 !important;
                    display: none !important;
                }

                .panel-arrowcontent {
                    opacity: 1 !important;
                    margin: 0 !important;
                }

                tab:only-of-type, tab:only-of-type + #tabs-newtab-button {
                    display: none !important;
                }

                #tabbrowser-tabs, #tabbrowser-arrowscrollbox {
                    min-height: 0 !important;
                }

                .titlebar-buttonbox-container{
                    position: fixed;
                    display:block !important;
                    top:0;
                    right:0;
                    height: 40px;
                }

                toolbar-menubar[inactive] > .titlebar-buttonbox-container{ opacity: 0 } 
                root[sidemode="maximized"] > #navigator-toolbox{ padding-top: 8px !important; } 
                root[sizemode="maximized"] .titlebar-buttonbox-container{ top: 8px } 
                root[uidensity="compact"] .titlebar-buttonbox-container{ height: 32px } 

                .titlebar-buttonbox-container > .titlebar-buttonbox{ height: 100%; }

                titlebar{
                    -moz-box-ordinal-group: 2;
                    -moz-appearance: none !important;
                }

                .titlebar-placeholder,
                TabsToolbar .titlebar-spacer{
                    display: none;
                } 

                /* Also hide the toolbox bottom border which isn't at bottom with this setup */
                navigator-toolbox::after{ display: none !important; } 

                toolbar-menubar .titlebar-buttonbox-container{ height: 22px; }
                toolbar-menubar .titlebar-button{ padding: 2px 17px !important; } 

                /* This will allow you to MAYBE put some items to the menubar */

                toolbar-menubar > *{ float: left }
                toolbar-menubar .toolbarbutton-1 { --toolbarbutton-inner-padding: 3px } 

                /* Makes tabs toolbar items zero-height initially and sets enlarge them to fill up space equal to tab-min-height set on tabs */ /* Firefox 65+ only */ /* !!USER!! - REMOVE ALL BUTTONS you can from the tabs toolbar */

                /* Configurable window drag space */

                root[sizemode="normal"] #nav-bar{ --window-drag-space-width: 20px }
                titlebar{ -moz-appearance: none !important; }
                tabbrowser-tabs, #tabbrowser-tabs > .tabbrowser-arrowscrollbox{
                    min-height: 0 !important;
                    max-height: 0 !important;
                } 

                root:not([customizing]) #tabbrowser-tabs .tabs-newtab-button, 
                root:not([customizing]) #TabsToolbar .titlebar-button{ 
                    -moz-appearance: none !important;
                    height: 0px;
                    padding-top: 0px !important;
                    padding-bottom: 0px !important;
                    -moz-box-align: stretch;
                    margin: 0 !important;
                }

                var(--tab-min-height, 0)

                tabbrowser-tabs .tabbrowser-tab{ height: var(--tab-min-height) }
                tabbrowser-tabs .tabbrowser-tab[first-visible-tab="true"][last-visible-tab="true"]{ 
                    visibility: collapse !important;
                }

                /* Extra top padding in maximized window */
                root[sizemode="maximized"] > #navigator-toolbox{ padding-top:7px !important; } 

                /* Fix window controls not being clickable */
                toolbar-menubar:hover{ 
                    min-height: calc(var(--tab-min-height) + var(--space-above-tabbar) - 1px) !important;
                    height: calc(var(--tab-min-height) + var(--space-above-tabbar) - 1px) !important;
                    -moz-appearance: initial !important;
                }

                nav-bar{ padding: 0 var(--window-drag-space-width,0px) } 

                #toolbar-menubar, #menubar-items, #main-menubar {
                    background-image: none !important;
                }

                #TabsToolbar, #menubar-items, #main-menubar {
                    background-image: none !important;
                }

            '';
            };
        };
    };
}

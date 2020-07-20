{ pkgs, ...}:
{
    programs.firefox = {
        enable = true;
        # extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        #     org-capture
        #     lastpass
        # ];

        profiles = {
            jake = {

                isDefault = true;

                settings = {
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
                html#main-window {
                    --color-bg: #21242b;
                    --toolbar-bgcolor: var(--color-bg) !important;

                    --gap-between-tabs: 5px;
                    --margin-after-tab-list: 3x;
                    --margin-before-tab-list: 3px;
                    --tab-font: "Cantarell", sans-serif;
                    --tab-font-weight: 700;
                    --tab-height: 20px;
                    --tabs-container-height: 70px;

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

                /* This positions the tabs under the navaigator container */
                #titlebar {
                    -moz-box-ordinal-group: 3 !important;
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
                    /* list-style-image: url("left-arrow.svg") !important; */
                    border: none !important;
                    color: var(--color-bg) !important;
                }

                #forward-button {
                    /* list-style-image: url("right-arrow.svg") !important; */
                    border: none !important;
                    color: var(--color-bg) !important;
                }

                #navigator-toolbox {
                    --tabs-border-color: var(--color-bg) !important;
                    background-color: var(--color-bg) !important;
                    border: none !important;
                    box-shadow: none !important;
                    max-height: var(		--tabs-container-height	) !important;
                    min-height: var(		--tabs-container-height	) !important;
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
                .tabbrowser-tab .tab-icon-image {
                    display: none !important;
                }

                /* Regular browser tabs */
                .tabbrowser-tab {
                    background-color: #1d2026 !important;
                    background-image: linear-gradient(to left, var(--color-tab-normal-start), var(--color-tab-normal-end)) !important;
                    border-radius: var(--tab-height) !important;
                    margin-inline-end: var(--gap-between-tabs) !important;
                    max-height: var(--tab-height) !important;
                    min-height: var(--tab-height) !important;
                    font-size: 12px !important;
                    color: white !important;
                }

                .tabbrowser-tab:hover {
                    background-color: none !important;
                    background-image: linear-gradient(to left, #c97dff, #ff8989) !important;
                    color: black !important;
                }

                .tabbrowser-tab[selected="true"] {
                    background-color: #51afef !important;
                    background-image: none !important;
                    color: black !important;
                    font-weight: var(--tab-font-weight) !important;
                }

                .tab-content {
                    background-color: none !important;
                    background-image: none !important;
                    font-family: var(--tab-font) !important;
                }

                /* pinned browser tabs */
                .tabbrowser-tab:hover[pinned="true"] {
                    background-image: linear-gradient(to left, #ff8989, #ff8989) !important;
                }

                .tabbrowser-tab[pinned="true"][selected="true"] {
                    background-image: linear-gradient(to left, #a1ffb6, #a1ffb6) !important;
                }

                .tabbrowser-tab[pinned="true"] {
                    background-image: linear-gradient(to left, #ffc386, #ffc386) !important;
                    color: #1d1d1d !important;
                    max-width: var(--tab-height) !important;
                    min-width: var(--tab-height) !important;
                }

                .tabbrowser-tab[pinned="true"] .tab-icon-image {
                    align-items: center !important;
                    display: inline-block !important;
                    min-height: var(--pinned-tab-favicon-dim) !important;
                    min-width: var(--pinned-tab-favicon-dim) !important;
                }

                .tabbrowser-tab[pinned="true"] .tab-label-container {
                    display: none !important;
                }

                .tab-stack {
                    display: flex !important;
                    justify-content: center !important;
                }

                /* Modify these values to tweak the start point of the tab list */
                .tabbrowser-arrowscrollbox {
                    margin-inline-start: var(--margin-before-tab-list) !important;
                    margin-inline-end: var(--margin-after-tab-list) !important;
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
            '';
            };
        };
    };
}

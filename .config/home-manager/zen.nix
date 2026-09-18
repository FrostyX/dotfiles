{ lib, pkgs, config, hostname, ... }:

let
  workSpaceId = "512caef5-f0d2-49d1-b38e-6e3f52ff9af8";
  personalSpaceId = "2c6c791a-9657-4d72-a7ac-92ea455a3bf5";
  zenProfilePath =
    if builtins.elem hostname [ "hive" "nova" ]
    then "FrostyX"
    else "hpgqitks.Default (release)";
  uiCustomization = builtins.toJSON {
    placements = {
      widget-overflow-fixed-list = [];
      unified-extensions-area = [
        "ublock0_raymondhill_net-browser-action"
        "_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action"
        "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action"
        "languagetool-webextension_languagetool_org-browser-action"
        "_8dd384e7-fc9e-4b6a-a744-497edc3408c3_-browser-action"
      ];
      nav-bar = [
        "back-button"
        "forward-button"
        "stop-reload-button"
        "personal-bookmarks"
        "vertical-spacer"
        "urlbar-container"
        "unified-extensions-button"
      ];
      toolbar-menubar = [ "menubar-items" ];
      TabsToolbar = [ "tabbrowser-tabs" ];
      vertical-tabs = [];
      PersonalToolbar = [];
      zen-sidebar-top-buttons = [ "home-button" ];
      zen-sidebar-foot-buttons = [
        "downloads-button"
        "zen-workspaces-button"
        "zen-create-new-button"
      ];
    };
    seen = [
      "_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action"
      "ublock0_raymondhill_net-browser-action"
      "ai-window-toggle"
      "developer-button"
      "screenshot-button"
      "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action"
      "languagetool-webextension_languagetool_org-browser-action"
    ];
    dirtyAreaCache = [
      "nav-bar"
      "vertical-tabs"
      "zen-sidebar-foot-buttons"
      "PersonalToolbar"
      "toolbar-menubar"
      "TabsToolbar"
      "unified-extensions-area"
      "zen-sidebar-top-buttons"
      "widget-overflow-fixed-list"
    ];
    currentVersion = 26;
    newElementCount = 2;
  };
in
lib.mkIf (builtins.elem hostname [ "pop-os" "nova" "hive" ]) {
  programs.zen-browser = {
    enable = true;
    configPath = if builtins.elem hostname [ "hive" "nova" ] then ".zen" else ".config/zen";
    env = {
      GTK_THEME = "catppuccin-mocha-mauve-standard";
    };
    policies = {
      AutofillAddressEnabled = true;
      OfferToSaveLogins = false;
      "3rdparty".Extensions."uBlock0@raymondhill.net" = {
        toOverwrite.filterLists = [
          "user-filters"
          "ublock-filters"
          "ublock-badware"
          "ublock-privacy"
          "ublock-unbreak"
          "ublock-quick-fixes"
          "easylist"
          "easyprivacy"
          "urlhaus-1"
          # EasyList and uBlock cookie notices.
          "fanboy-cookiemonster"
          "ublock-cookies-easylist"
        ];
      };
      ExtensionSettings = {
        "uBlock0@raymondhill.net" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          installation_mode = "force_installed";
          private_browsing = true;
        };
        "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi";
          installation_mode = "force_installed";
        };
        "{d7742d87-e61d-4b78-b8a1-b469842139fa}" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi";
          installation_mode = "force_installed";
        };
        "languagetool-webextension@languagetool.org" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/languagetool/latest.xpi";
          installation_mode = "force_installed";
        };
        "{8dd384e7-fc9e-4b6a-a744-497edc3408c3}" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/edit-with-emacs/latest.xpi";
          installation_mode = "force_installed";
        };
      };
    };
    profiles.default = {
      isDefault = true;
      containersForce = true;
      containers.Stream = {
        id = 6;
        color = "pink";
        icon = "circle";
      };
      name =
        if builtins.elem hostname [ "hive" "nova" ]
        then "FrostyX"
        else "default";
      path =
        if (hostname == "pop-os")
        then "default"
        else zenProfilePath;

      presets.catppuccin = {
          enable = true;
          # Frappe | Latte | Macchiato | Mocha
          flavor = "Mocha";
          # Blue, Flamingo, Green, Lavender, Maroon, Mauve, ...
          accent = "Mauve";
      };

      bookmarks = {
        force = true;
        settings = [
          {
            name = "toolbar";
            toolbar = true;
            bookmarks = [
              {
                name = "Abathur";
                url = "http://192.168.1.222/";
              }
              {
                name = "Game Time";
                url = "https://eu.shop.battle.net/en-gb/product/world-of-warcraft-game-time";
              }
              {
                name = "Messages";
                url = "https://www.instagram.com/direct/inbox/";
              }
              {
                name = "Spotify";
                url = "https://open.spotify.com/browse/featured";
              }
              {
                name = "Netflix";
                url = "https://www.netflix.com/browse";
              }
              {
                name = "JIRA";
                url = "https://redhat.atlassian.net/jira/software/c/projects/CPT/boards/7400/backlog";
              }
              {
                name = "Gmail";
                url = "https://mail.google.com/mail/u/1/#inbox";
              }
              {
                name = "Calendar";
                url = "https://calendar.google.com/calendar/u/1/r";
              }
              {
                name = "AWS";
                url = "https://id.fedoraproject.org/saml2/SSO/Redirect?SPIdentifier=urn:amazon:webservices&RelayState=https://console.aws.amazon.com";
              }
              {
                name = "GitHub";
                url = "https://github.com/FrostyX";
              }
            ];
          }
        ];
      };

      spaces = {
        Work = {
          id = workSpaceId;
          position = 0;
          icon = "❤️";
        };
        Personal = {
          id = personalSpaceId;
          position = 1;
          icon = "💙";
        };
      };
      spacesForce = true;

      settings = {
        "browser.uiCustomization.state" = uiCustomization;
        "browser.startup.homepage" = "https://google.com";

        # Allow Kerberos/GSSAPI authentication for these sites
        "network.negotiate-auth.trusted-uris" = ".redhat.com";
        "network.negotiate-auth.delegation-uris" = ".redhat.com";

        # Zen is a Nix-built binary and cannot find the system GSSAPI library
        # (/usr/lib64 is not on its library path). Point it at the
        # Nix-provided libgssapi instead.
        "network.negotiate-auth.gsslib" =
          "${pkgs.libkrb5.lib}/lib/libgssapi_krb5.so.2";

        "general.smoothScroll.msdPhysics.enabled" = false;
        "zen.theme.content-element-separation" = 0;
        "zen.theme.hide-unified-extensions-button" = true;
        "zen.glance.enabled" = false;
        "zen.view.show-newtab-button-top" = false;
        "zen.view.use-single-toolbar" = false;
        "zen.welcome-screen.seen" = true;
        "media.videocontrols.picture-in-picture.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "layout.css.prefers-color-scheme.content-override" = 1;
        "media.ffmpeg.vaapi.enabled" = true;
        "gfx.x11-egl.force-enabled" = true;

        # Geolocation seems to be tricky. Without these settings, Zen won't
        # even ask for permissions to find me on a map. With them, it finds me,
        # even though on an incorrect location. It is weird, because Firefox
        # from the host system finds me on the map perfectly.
        # The key is taken from the official Fedora package
        # https://src.fedoraproject.org/rpms/firefox/blob/rawhide/f/google-loc-api-key
        "browser.translations.neverTranslateLanguages" = "cs,en,sk";
        "geo.provider.use_geoclue" = false;
        "geo.provider.network.url" = "https://www.googleapis.com/geolocation/v1/geolocate?key=AIzaSyB2h2OuRcUgy5N-5hsZqiPW6sH3n_rptiQ";
      };

      userChrome = ''
        @import "catppuccin/userChrome.css";

        /* Stream container: Catppuccin Mocha pink. */
        [usercontextid="${toString config.programs.zen-browser.profiles.default.containers.Stream.id}"] {
          --identity-tab-color: #f5c2e7 !important;
          --identity-icon-color: #f5c2e7 !important;
        }

        #zen-sidebar-top-buttons {
          background: var(--zen-themed-toolbar-bg) !important;
        }

        /* TODO: Replace this CSS workaround with Zen's native workspace bookmark
           assignments once they can be configured declaratively.
           Show only the active space's bookmarks on the toolbar. */

        :root:not(:has(zen-workspace[id="{${personalSpaceId}}"][active]))
          #personal-bookmarks .bookmark-item:is(
            [label="Abathur"], [label="Game Time"],
            [label="Spotify"], [label="Netflix"], [label="Messages"],
          ) {
          display: none !important;
        }

        :root:has(zen-workspace[id="{${personalSpaceId}}"][active])
          #personal-bookmarks .bookmark-item:is(
            [label="JIRA"], [label="Gmail"], [label="Calendar"],
            [label="AWS"], [label="GitHub"],
          ) {
          display: none !important;
        }
      '';
      userContent = builtins.readFile ../../.zen/FrostyX/userContent.css;
    };
  };
}

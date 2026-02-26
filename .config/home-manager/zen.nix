{ lib, config, hostname, ... }:

lib.mkIf (builtins.elem hostname [ "pop-os" "nova" ]) {
  home.file.".zen/profiles.ini".force = true;
  home.file.".zen/hpgqitks.Default (release)/user.js".force =
    lib.mkIf (hostname == "nova") true;

  programs.zen-browser = {
    enable = true;
    policies = {
      AutofillAddressEnabled = true;
    };
    profiles.default = {
      isDefault = true;
      path =
        if (hostname == "nova")
        then "hpgqitks.Default (release)"
        else if (hostname == "hive")
        then "default"
        else "default";

      # settings = {
      #   "browser.startup.homepage" = "https://google.com";
      #   "general.smoothScroll.msdPhysics.enabled" = false;
      #   "zen.theme.content-element-separation" = 0;
      #   "zen.glance.enabled" = false;
      #   "zen.view.show-newtab-button-top" = false;
      #   "zen.view.use-single-toolbar" = false;
      #   "zen.welcome-screen.seen" = true;
      # };


      settings = let
        uiCustomization = builtins.toJSON {
          placements = {
            widget-overflow-fixed-list = [];
            unified-extensions-area = [
              "ublock0_raymondhill_net-browser-action"
              "_testpilot-containers-browser-action"
              "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action"
              "languagetool-webextension_languagetool_org-browser-action"
              "idcac-pub_guus_ninja-browser-action"
              "_8dd384e7-fc9e-4b6a-a744-497edc3408c3_-browser-action"
              "_c2c003ee-bd69-42a2-b0e9-6f34222cb046_-browser-action"
            ];
            nav-bar = [
              "back-button"
              "forward-button"
              "stop-reload-button"
              "personal-bookmarks"
              "vertical-spacer"
              "urlbar-container"
              "customizableui-special-spring2"
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
            "developer-button"
            "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action"
            "languagetool-webextension_languagetool_org-browser-action"
            "idcac-pub_guus_ninja-browser-action"
            "ublock0_raymondhill_net-browser-action"
            "_8dd384e7-fc9e-4b6a-a744-497edc3408c3_-browser-action"
            "_testpilot-containers-browser-action"
            "_c2c003ee-bd69-42a2-b0e9-6f34222cb046_-browser-action"
            "screenshot-button"
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
          currentVersion = 22;
          newElementCount = 13;
        };
      in {
        "browser.uiCustomization.state" = uiCustomization;
        "browser.startup.homepage" = "https://google.com";
        "general.smoothScroll.msdPhysics.enabled" = false;
        "zen.theme.content-element-separation" = 0;
        "zen.glance.enabled" = false;
        "zen.view.show-newtab-button-top" = false;
        "zen.view.use-single-toolbar" = false;
        "zen.welcome-screen.seen" = true;
      };

      userChrome = builtins.readFile (../../.zen + "/hpgqitks.Default (release)/chrome/userChrome.css");
      userContent = builtins.readFile (../../.zen + "/hpgqitks.Default (release)/chrome/userContent.css");
    };
  };
}



  # gtk = {
  #   enable = true;
  #   # See ~/.config/gtk-3.0/settings.ini
  #   # theme.name = "";
  #   # cursorTheme.name = "";
  #   iconTheme.name = "breeze";
  # };

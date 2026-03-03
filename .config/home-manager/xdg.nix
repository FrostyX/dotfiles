{ ... }:

{
  # You might need to manually run
  # rm /home/jkadlcik/.local/share/applications/mimeapps.list
  xdg = {
    mimeApps.enable = true;
    configFile."mimeapps.list".force = true;

    # See /home/jkadlcik/.local/share/applications/mimeapps.list
    mimeApps.defaultApplications = {
      "text/plain" = "org.kde.kwrite.desktop";
      "text/html" = "zen-browser.desktop";
      "image/jpeg" = "org.gnome.eog.desktop";
      "image/png" = "org.gnome.eog.desktop";
      "image/webp" = "org.gnome.eog.desktop";
      "image/svg+xml" = "org.inkscape.Inkscape.desktop";
      "video/mp4" = "vlc.desktop";
      "video/quicktime" = "vlc.desktop";
      "video/x-msvideo" = "vlc.desktop";
      "video/x-matroska" = "vlc.desktop";
      "application/pdf" = "okularApplication_pdf.desktop";
      "x-scheme-handler/http" = "zen-browser.desktop";
      "x-scheme-handler/https" = "zen-browser.desktop";
    };
  };
}

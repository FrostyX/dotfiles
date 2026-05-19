{
  services.flatpak = {
    enable = true;
    packages = [
      "com.spotify.Client"
      "com.slack.Slack"
      "com.discordapp.Discord"
      "engineer.atlas.Nyxt"
      "com.github.iwalton3.jellyfin-media-player"
      "im.riot.Riot"
    ];
  };
}

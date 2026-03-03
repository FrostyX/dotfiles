{ ... }:

{
  programs.git = {
    enable = true;

    # See ~/.config/git/config
    settings = {
      user = {
        name = "Jakub Kadlcik";
        email = "frostyx@email.cz";
      };
      pull = {
        rebase = true;
      };
      alias = {
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        forcepull = "!git reset --hard main && git pull --rebase";
        foo = "log";
      };
    };
  };
}

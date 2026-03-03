{ ... }:

{
  programs.git = {
    enable = true;

    # See ~/.config/git/config
    settings = {
      core = {
        editor = "vim";
      };
      pull = {
        rebase = true;
      };
      init = {
        defaultBranch = "main";
      };
      user = {
        name = "Jakub Kadlcik";
        email = "frostyx@email.cz";
      };
      github = {
        user = "frostyx";
      };
      pagure = {
        user = "frostyx";
      };
      alias = {
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
        forcepull = "!git reset --hard main && git pull --rebase";
        foo = "log";
      };
    };
  };
}

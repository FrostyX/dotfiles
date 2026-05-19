# dotfiles

## Prerequisites

```
sudo dnf install nix
sudo systemctl enable nix-daemon --now
```

## Installation

    # Choose method which suits better for you
    git clone git@github.com:FrostyX/dotfiles.git ~/.dotfiles
    git clone https://github.com/FrostyX/dotfiles.git ~/.dotfiles

    # Optionally symlink dotfiles next to other git projects
    ln -s ~/.dotfiles ~/git/dotfiles

    nix run .#playbook -- -K
    nix run nixpkgs#home-manager -- \
        switch --flake ~/.dotfiles/#jkadlcik@nova -b backup

    # Tmux
    mkdir -p ~/.tmux/plugins ~/.tmux/resurrect
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ~/.tmux/plugins/tpm/scripts/install_plugins.sh
    tmux source-file ~/.tmux.conf

    # Vim
    vim +PluginInstall +qall

# dotfiles

## Installation

    # Choose method which suits better for you
    git clone git@github.com:FrostyX/dotfiles.git ~/.dotfiles
    git clone https://github.com/FrostyX/dotfiles.git ~/.dotfiles

    # Optionally symlink dotfiles next to other git projects
    ln -s ~/.dotfiles ~/git/dotfiles

    # Make symlinks for configuration files
    stow . -d ~/.dotfiles/ -t ~

    # Get git submodules (required for Vundle)
    cd ~/.dotfiles
    git submodule update --init --recursive

    # Tmux
    mkdir -p ~/.tmux/plugins ~/.tmux/resurrect
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ~/.tmux/plugins/tpm/scripts/install_plugins.sh
    tmux source-file ~/.tmux.conf

    # Vim
    vim +PluginInstall +qall


## Remove dotfiles

    stow -D . -d ~/.dotfiles/ -t ~

## Add a new file

    mv ~/.foo ~/.dotfiles/
    ln -s ~/.dotfiles/.foo ~/

## Playbooks

I use ansible playbooks to bootstrap my new workstations
https://github.com/FrostyX/playbooks

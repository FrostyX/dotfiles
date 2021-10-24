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

## Remove dotfiles

    stow -D . -d ~/.dotfiles/ -t ~

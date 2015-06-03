# dotfiles
## Installation

	# Choose method which suits better for you
	git clone git@github.com:FrostyX/dotfiles.git ~/.dotfiles
	git clone https://github.com/FrostyX/dotfiles.git ~/.dotfiles

	# Make symlinks for configuration files
	ln -s ~/.dotfiles/.vim ~/.vim
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	
	ln -s ~/.dotfiles/.config/qtile ~/.config/qtile

	# Get git submodules
	cd ~/.dotfiles
	git submodule update --init --recursive

## Update submodules

	git submodule update

## VIM

	# Run every time when new Vundle module is added
	vim +PluginInstall +qall

### YouCompleteMe
YouCompleteMe is a fast, as-you-type, fuzzy-search code completion engine for Vim. It's the best choice for using Vim as IDE, but so bad for portability.

When you want portable configuration, just dont install YouCompleteMe. It will be used alternative tools providing similar feature as YouCompleteMe.

Requires:

- C++ compiler (Clang for semantic completion, else g++)
- CMake & Make
- About 15 minutes


Installation:

	cd ~/.dotfiles/.vim/bundle/YouCompleteMe

	# For semantic completion
	./install.sh --clang-completer

	# Or without semantic completion
	./install.sh

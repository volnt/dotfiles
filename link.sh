#!/bin/bash

# Emacs
ln -fs $PWD/.emacs $HOME/.emacs

# Urxvt
ln -fs $PWD/.Xdefaults $HOME/.Xdefaults

# Fish
mkdir -p $HOME/.config/fish/functions/
ln -fs $PWD/fish/functions/*.fish $HOME/.config/fish/functions/

# Awesome
mkdir -p $HOME/.config/awesome/
ln -fs $PWD/awesome/* $HOME/.config/awesome/

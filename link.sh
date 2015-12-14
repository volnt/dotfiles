#!/bin/bash

# Emacs
ln -s $PWD/.emacs $HOME/.emacs

# Urxvt
ln -s $PWD/.Xdefaults $HOME/.Xdefaults

# Fish
mkdir -p $HOME/.config/fish/functions/
ln -s $PWD/fish/functions/*.fish $HOME/.config/fish/functions/

# Awesome
mkdir -p $HOME/.config/awesome/
ln -s $PWD/awesome/* $HOME/.config/awesome/

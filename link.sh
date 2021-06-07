#!/bin/bash

# Emacs
ln -fs $PWD/emacs.d $HOME/.emacs.d

# Urxvt
ln -fs $PWD/.Xdefaults $HOME/.Xdefaults

# Awesome
mkdir -p $HOME/.config/awesome/
ln -fs $PWD/awesome/* $HOME/.config/awesome/

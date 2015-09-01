#!/bin/bash

ln -s $PWD/.emacs $HOME/.emacs
ln -s $PWD/.Xdefaults $HOME/.Xdefaults

mkdir -p $HOME/.config/fish/functions/
ln -s $PWD/fish/functions/*.fish $HOME/.config/fish/functions/

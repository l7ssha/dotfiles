#!/bin/sh

currentdir="$(pwd)"

echo "Linking emacs config..."
ln -fsT "$currentdir/emacsconfig.el" ~/.emacs.d/init.el

echo "Linking zshell config..."
ln -sfT "$currentdir/.zshrc" ~/.zshrc

echo "Linking .gitconfig..."
ln -sfT "$currentdir/.gitconfig" ~/.gitconfig

echo "Done. Config installed successfully."

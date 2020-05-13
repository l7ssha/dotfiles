#!/bin/sh

currentdir="$(pwd)"

echo "Linking emacs config..."
ln -fsT "$(pwd)/emacsconfig.el" ~/.emacs.d/init.el

echo "Linking zshell config..."
ln -sfT "$(pwd)/.zshrc" ~/.zshrc

echo "Linking .gitconfig..."
ln -sfT "$(pwd)/.gitconfig" ~/.gitconfig

echo "Done. Config installed successfully."

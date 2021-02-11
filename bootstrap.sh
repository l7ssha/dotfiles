#!/bin/bash

currentdir="$(pwd)"

echo "Installing config..."

ln -sfT "$currentdir/.zshrc" ~/.zshrc
ln -sfT "$currentdir/.gitconfig" ~/.gitconfig

echo "Switching to unstable branch and updating mirrors..."
sudo pacman-mirrors --api --set-branch unstable
sudo pacman-mirrors --fasttrack 5

echo "Updating system..."
sudo pacman -Syyu

echo "Installing packages from official repositories..."
sudo pacman -S base-devel discord brave zerotier-one docker docker-compose \
    libreoffice-fresh obs-studio bitwarden lutris nvtop xclip zsh-autosuggestions \
    lsd --confirm

parubuilddir="/tmp/paru"
git clone https://aur.archlinux.org/paru.git "$parubuilddir"
makepkg -si BUILDDIR="$parubuilddir"

echo "Installing packages from AUR..."
paru -S paru-git spotify-dev multimc5 intellij-idea-ultimate-edition \
    intellij-idea-ultimate-edition-jre teams antigen-git vscodium-bin \
    kwin-decoration-sierra-breeze-enhanced-git

echo "Enabling and starting services..."
sudo systemctl enable --now docker
sudo systemctl enable --now zerotier-one

echo "Adding user to necessary groups..."
sudo usermod -a -G docker,uucp,tty,vboxusers $USER

echo "Changing shell to zsh..."
chsh -s /bin/zsh $USER

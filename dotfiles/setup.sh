#!/bin/zsh
git init --bare $HOME/dotfiles
alias dotfilesgit='/usr/local/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'
dotfilesgit config --local status.showUntrackedFiles no
echo "alias dotfilesgit='/usr/local/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'" >> $HOME/.zshrc
echo "alias dfsg='/usr/local/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'" >> $HOME/.zshrc


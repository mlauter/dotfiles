# dotfiles
my config files

## For easy setup

1. set zsh to default shell and install oh-my-zsh `sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
`
1. backup all existing dotfiles (gitconfig, zshrc, tmux.conf, .spacemacs, custom zsh theme)
1. clone this repo
1. `chmod +x dotfiles/setup.sh` (This will create the bare git directory and setup the dfsg alias for using git to interact with this special repo)
1. If exists: `rm ~/.oh-my-zsh/custom/themes/my-robbyrussell.zsh-theme`
1. `cp ~/.my-robbyrussell.zsh-theme ~/.oh-my-zsh/custom/themes/my-robbyrussell.zsh-theme`
2. `take ~/dotfiles_local`
1. `touch .gitconfig_local` Add stuff that needs to stay local here
1. see https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

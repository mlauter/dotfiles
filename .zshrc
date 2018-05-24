# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH=/home/mlauter/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
  ZSH_THEME="my-robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions shrink-path Z wd kubectl)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR=""

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

cd ~/development/Etsyweb
eval $(dbaliases)
source ~/development/bin/xdebug_toggle

# source fzf for fuzzy history searching
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export TERM=xterm-256color
export CLASSPATH=/home/mlauter/irccat
export PATH=$PATH:/home/mlauter/development/bin
export TZ='America/New_York'

GIT_PROMPT_RODEO="🦄"

bindkey "^[^[[C" forward-word
bindkey "^[^[[D" backward-word
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

alias gdiff='gist -t diff'
alias ll='ls -al'
alias lb='ls -B -I "#*#"'
alias etsyphp='sudo -u apache ETSY_ENVIRONMENT=development php'
alias etsy_aux='dbconnect etsy_aux_A'
alias db_vertica='dbconnect vertica'
alias db_atlas='dbconnect etsy_atlas_A'
alias cdew='cd ~/development/Etsyweb'
alias gl='git log --graph --pretty="format:%C(yellow)%h%Cblue%d%Creset %s %C(white)"''""'
alias gderp='git add --all && git commit -m "derp" && git rebase -i'
alias g='git'

alias gcp='git cherry-pick'
alias gco='git checkout'

alias e="emacsclient -t"
alias blink='printf "[5m%s[0m"'

alias vertica='/opt/vertica/bin/vsql -U mlauter -h vertica-prod -w VjVvYJJXWWGZZqDBxdYhEfaBGQrgV7Kdi vbit'
alias wrangle='php ~/development/Etsyweb/bin/rodeo/wrangle.php'

# pbcopy () {
#     ssh `echo $SSH_CLIENT | awk '{print $1}'` pbcopy;

# }

# pbpaste () {
#     ssh `echo $SSH_CLIENT | awk '{print $1}'` pbpaste;

# }

omnichef () {
    git rpull && knife spork omni "$@" && git commit --amend && git rpull && git push && knife spork promote "$@" --remote

}

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
source /usr/bin/virtualenvwrapper.sh
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

export PATH=$PATH:/home/mlauter/bin

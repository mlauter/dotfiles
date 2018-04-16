# Default unless overriden in .zshrc
GIT_PROMPT_RODEO="🐴"

prompt_dir() {
    echo -n "%{$fg[cyan]%}$(shrink_path -f)%{$reset_color%}"
}

# Status:
# - am I root
# - have i changed rodeo files
prompt_status() {
  local symbols
  symbols=()
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}⚡"
  # wrangle will be upset if this is run outside a git repo
  # if [[ -n $(command git status ${FLAGS} 2> /dev/null | tail -n1) ]]; then
  #   if ! php ~/development/Etsyweb/bin/rodeo/wrangle.php > /dev/null; then
  #     symbols+="%{%F{yellow}%}$GIT_PROMPT_RODEO"
  #   fi
  # fi

  [[ -n "$symbols" ]] && echo -n "$symbols  "
}

parse_git_dirty() {
  local INDEX FLAGS STATUS
  INDEX=$(command git status --porcelain -b 2> /dev/null)
  STATUS=''
  FLAGS=('--porcelain')

  if [[ "$(command git config --get oh-my-zsh.hide-dirty)" != "1" ]]; then
    if [[ $POST_1_7_2_GIT -gt 0 ]]; then
      FLAGS+='--ignore-submodules=dirty'
    fi
    if [[ "$DISABLE_UNTRACKED_FILES_DIRTY" == "true" ]]; then
      FLAGS+='--untracked-files=no'
    fi
    STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
  fi
  if [[ -n $STATUS ]]; then
    # if ! php ~/development/Etsyweb/bin/rodeo/wrangle.php > /dev/null; then
    #     echo "%{%F{yellow}%}$GIT_PROMPT_RODEO "
    # if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
      #     echo "$ZSH_THEME_GIT_PROMPT_ADDED"
    local COUNT_ADDED COUNT_DIRTY
    COUNT_ADDED=$(echo "$INDEX" | grep '^M' | wc -l 2> /dev/null)
    COUNT_DIRTY=$(echo "$INDEX" | egrep '^M| M' | wc -l 2> /dev/null)

    # Dirty hack because of untracked files
    if [[ $COUNT_DIRTY == 0 ]]; then
        echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
    elif [[ $COUNT_ADDED == $COUNT_DIRTY ]]; then
        echo "$ZSH_THEME_GIT_PROMPT_ADDED"
    else
        echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
    fi
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
PROMPT='$(prompt_status)${ret_status} $(prompt_dir) $(git_prompt_info)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[blue]%}) %{$fg[yellow]%}●"

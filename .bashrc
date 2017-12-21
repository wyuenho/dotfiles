export MBOX
MBOX="$HOME/.mail/mbox"

export EDITOR
EDITOR="emacs -Q -mm"

# Aliases
if [ "$(which ls)" == "/bin/ls" ]; then
    alias ls="ls -FhG"
else
    alias ls="ls -Fh --color=auto"
fi

alias grep="grep --color=auto -C 5 -n -H"
alias cp="cp -iPp"
alias mv="mv -i -v"
alias su="/usr/bin/su"
alias sudo="/usr/bin/sudo"
alias diff="diff -u -B -r"

# Prompt
if [ -f $HOME/.scm-prompt.sh ]; then
    source "$HOME/.scm-prompt.sh"
    PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[01;35m\]$(_scm_prompt)\[\e[0m\]\n\$ '
else
    PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '
fi

bind 'set match-hidden-files off'

# Bash Completion
if [ -f /opt/local/etc/bash_completion ]; then # MacPorts
    source /opt/local/etc/bash_completion
elif [ -f /usr/local/etc/bash_completion ]; then # Homebrew
    source /usr/local/etc/bash_completion
fi

# gcloud
if [ -f "$HOME/.google-cloud-sdk/completion.bash.inc" ]; then
    source "$HOME/.google-cloud-sdk/completion.bash.inc";
fi

# Better bash history search
export HH_CONFIG=hicolor,rawhistory # get more colors
shopt -s histappend                 # append new history items to .bash_history
export HISTCONTROL=ignorespace      # leading space hides commands from history
export HISTFILESIZE=10000           # increase history file size (default is 500)
export HISTSIZE=${HISTFILESIZE}     # increase history size (default is 500)
export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"   # mem/file sync
# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* && "$(type -fp hh)" ]]; then bind '"\C-r": "\C-a hh -- \C-j"'; fi

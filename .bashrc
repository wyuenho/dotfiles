export MBOX
MBOX="$HOME/.mail/mbox"

export EDITOR
EDITOR="emacsclient"

export ALTERNATE_EDITOR
ALTERNATE_EDITOR="emacs"

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
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) ]]; then
    if [ -r /opt/local/etc/bash_completion ]; then # MacPorts
        source /opt/local/etc/bash_completion
    elif [ -r /usr/local/etc/bash_completion ]; then # Homebrew
        source /usr/local/etc/bash_completion
    fi

    # NVM
    [ -r "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

    # GVM
    [ -r "$HOME/.gvm/scripts/completion" ] && source "$HOME/.gvm/scripts/completion"

    # gcloud
    if [ -f "$HOME/.google-cloud-sdk/completion.bash.inc" ]; then
        source "$HOME/.google-cloud-sdk/completion.bash.inc";
    fi
fi


# Better bash history search
HH_CONFIG=hicolor,rawhistory # get more colors
export HH_CONFIG

# append new history items to .bash_history
shopt -s histappend

# leading space hides commands from history
HISTCONTROL=ignorespace
export HISTCONTROL

# increase history file size (default is 500)
HISTFILESIZE=10000
export HISTFILESIZE

# increase history size (default is 500)
HISTSIZE=${HISTFILESIZE}
export HISTSIZE

# mem/file sync
PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"
# Magic fix for command not found error lifted from
# https://github.com/Bash-it/bash-it/pull/709
declared="$(declare -p PROMPT_COMMAND)"
[[ "$declared" =~ \ -[aAilrtu]*x[aAilrtu]*\  ]] 2>/dev/null
[[ $? -eq 0 ]] && export PROMPT_COMMAND

# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* && "$(type -fp hh)" ]]; then bind '"\C-r": "\C-a hh -- \C-j"'; fi

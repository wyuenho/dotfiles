export MBOX
MBOX="$HOME/.mail/mbox"

export EDITOR
EDITOR="emacsclient"

export ALTERNATE_EDITOR
ALTERNATE_EDITOR="emacs"

export HGMERGE
HGMERGE="emacsclient"

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

[ "$(type -fp hub)" ] && eval "$(hub alias -s)"

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
    if [ -f /opt/local/etc/bash_completion ]; then # MacPorts
        source /opt/local/etc/bash_completion
    elif [ -f /usr/local/etc/bash_completion ]; then # Homebrew
        source /usr/local/etc/bash_completion
    fi

    # NVM
    [ -f "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

    # GVM
    [ -f "$HOME/.gvm/scripts/completion" ] && source "$HOME/.gvm/scripts/completion"

    # gcloud
    if [ -f "$HOME/.google-cloud-sdk/completion.bash.inc" ]; then
        source "$HOME/.google-cloud-sdk/completion.bash.inc";
    fi
fi

# Direnv
if [ -f "$(type -fp direnv)" ]; then
    eval "$(direnv hook bash)"
fi

# Better bash history search
[ "$(type -fp hstr)" ] && alias hh="hstr"

HSTR_CONFIG=hicolor,raw-history-view
export HSTR_CONFIG

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

if [[ $- =~ .*i.* && "$(type -fp hstr)" ]]; then
    # if this is interactive shell, then bind hstr to Ctrl-r (for Vi mode check doc)
    bind '"\C-r": "\C-a hstr -- \C-j"';
    # if this is interactive shell, then bind 'kill last command' to Ctrl-x k
    bind '"\C-xk": "\C-a hstr -k \C-j"';
fi

# The next line updates PATH for Netlify's Git Credential Helper.
if [ -f '/Users/jimmywong/.netlify/helper/path.bash.inc' ]; then source '/Users/jimmywong/.netlify/helper/path.bash.inc'; fi

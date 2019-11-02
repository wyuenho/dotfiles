# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export MBOX
MBOX="$HOME/.mail/mbox"

if [ -x emacs ]; then
    export EDITOR
    EDITOR="emacsclient -t"

    export ALTERNATE_EDITOR
    ALTERNATE_EDITOR="emacs"

    export HGMERGE
    HGMERGE="$EDITOR"
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls="ls -Fh --color=auto"
else
    alias ls="ls -FhG"
fi

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias grep='grep --color=auto -C 5 -nH'
alias fgrep='grep -F'
alias egrep='grep -E'

alias cp="cp -iPp"
alias mv="mv -iv"

if diff --color /etc/hostname /etc/hostname > /dev/null 2>&1; then
    alias diff="diff --color -ur"
else
    alias diff="diff -ur"
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

[ "$(type -fp hub)" ] && eval "$(hub alias -s)"

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# Prompt
if [ -f "$HOME/.local/bin/scm-prompt.sh" ]; then
    source "$HOME/.local/bin/scm-prompt.sh"
fi

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[01;35m\]$([ $(type -t _scm_prompt) ] && _scm_prompt)\[\e[0m\]\n\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h \w$([ $(type -t _scm_prompt) ] && _scm_prompt)\n\$ '
fi
unset color_prompt

# Bash Completion
if [ -z "$INSIDE_EMACS" ] || [ "$EMACS_BASH_COMPLETE" = "t" ] && ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then # Debian
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    elif [ -f /opt/local/etc/bash_completion ]; then # MacPorts
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
if "$declared" =~ \ -[aAilrtu]*x[aAilrtu]*\  2>/dev/null; then
    export PROMPT_COMMAND
fi

if [[ $- =~ .*i.* && "$(type -fp hstr)" ]]; then
    # if this is interactive shell, then bind hstr to Ctrl-r (for Vi mode check doc)
    bind '"\C-r": "\C-a hstr -- \C-j"';
    # if this is interactive shell, then bind 'kill last command' to Ctrl-x k
    bind '"\C-xk": "\C-a hstr -k \C-j"';
fi

# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
    # If set, the pattern "**" used in a pathname expansion context will
    # match all files and zero or more directories and subdirectories.
    shopt -s globstar
fi

export MBOX
MBOX="$HOME/.mail/mbox"

if [ -x "$(command -v emacs)" ]; then
    export EDITOR
    EDITOR="emacsclient"

    export VISUAL
    VISUAL="$EDITOR"

    export ALTERNATE_EDITOR
    ALTERNATE_EDITOR=

    export SUDO_EDITOR
    SUDO_EDITOR="$EDITOR"

    export HGMERGE
    HGMERGE="$EDITOR"
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # Debian

# Aliases
[ -f ~/.bash_aliases ] && source ~/.bash_aliases

# Prompt
# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[01;35m\]\[\e[0m\]\n\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h \w\n\$ '
fi
unset color_prompt

# Bash Completion
if [ -z "$INSIDE_EMACS" ] || [ "$INSIDE_EMACS" = "vterm" ] || [ "$EMACS_BASH_COMPLETE" = "t" ] && ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then # Debian
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    elif [ -f /opt/local/etc/bash_completion ]; then # MacPorts
        source /opt/local/etc/bash_completion
    elif [ -f /usr/local/etc/bash_completion ]; then # Homebrew
        source /usr/local/etc/bash_completion
    fi

    # nvm
    if [ -d "$NVM_DIR" ] && [ -f "$NVM_DIR/bash_completion" ]; then
        source "$NVM_DIR/bash_completion"
    fi

    # gvm
    [ -f "$HOME/.gvm/scripts/completion" ] && source "$HOME/.gvm/scripts/completion"

    # sdkman!
    [ -x "$(command -v sdk)" ] && eval "$(sdk completion bash)"

    # gcloud
    if [ -f "$HOME/.google-cloud-sdk/completion.bash.inc" ]; then
        source "$HOME/.google-cloud-sdk/completion.bash.inc";
    fi

    # aws
    [ -x "$(command -v aws_completer)" ] && complete -C "$(type -p aws_completer)" aws

    # pyenv
    if [ -d "$PYENV_ROOT" ] && [ -f "$PYENV_ROOT/completions/pyenv.bash" ]; then
        source "$PYENV_ROOT/completions/pyenv.bash"
    fi

    # git-subrepo
    if [ -d "$GIT_SUBREPO_ROOT" ] && [ -f "$GIT_SUBREPO_ROOT/share/completion.bash" ]; then
        source "$GIT_SUBREPO_ROOT/share/completion.bash"
    fi

    # CircleCI
    [ -x "$(command -v circleci)" ] && eval "$(circleci completion bash)"
fi

# Direnv
if [ -x "$(command -v direnv)" ]; then
    eval "$(direnv hook bash)"
fi

# pyenv
if [ -x "$(command -v pyenv)" ]; then
    export PYENV_SHELL=bash
    command pyenv rehash 2>/dev/null
    pyenv() {
        local command
        command="${1:-}"
        if [ "$#" -gt 0 ]; then
            shift
        fi

        case "$command" in
            activate|deactivate|rehash|shell)
                eval "$(pyenv "sh-$command" "$@")"
                ;;
            *)
                command pyenv "$command" "$@"
                ;;
        esac
    }
fi

# History

# append new history items to .bash_history
shopt -s histappend

# leading space hides commands from history, move dups to front and erase old
# entry
HISTCONTROL=ignorespace:erasedups
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

# Better bash history search
if [ -r "/opt/local/share/mcfly/mcfly.bash" ]; then
    export MCFLY_FUZZY
    MCFLY_FUZZY=true
    export MCFLY_RESULTS
    MCFLY_RESULTS=50
    source "/opt/local/share/mcfly/mcfly.bash"
fi

# Feature packed and fast prompt
if [ -x "$(command -v starship)" ]; then
    eval "$(starship init bash)"
fi

# Emacs vterm support
vterm_printf() {
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

PS1=$PS1'\[$(vterm_prompt_end)\]'

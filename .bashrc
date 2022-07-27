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

if [ -x "$(type -P emacs)" ]; then
    export EDITOR
    EDITOR="emacsclient -n"

    export VISUAL
    VISUAL="$EDITOR"

    export ALTERNATE_EDITOR
    ALTERNATE_EDITOR=

    export SUDO_EDITOR
    SUDO_EDITOR="$EDITOR"

    export HGMERGE
    HGMERGE="$EDITOR"
fi

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)" # Debian

# Colorful man pages
# blink
LESS_TERMCAP_mb=$(tput blink; tput setaf 1)
export LESS_TERMCAP_mb

# standout
LESS_TERMCAP_so=$(tput smso; tput setaf 3; tput setab 7)
export LESS_TERMCAP_so
LESS_TERMCAP_se=$(tput rmso; tput op)
export LESS_TERMCAP_se

# bold
LESS_TERMCAP_md=$(tput bold; tput setaf 5)
export LESS_TERMCAP_md

# italic
LESS_TERMCAP_ZH=$(tput sitm; tput setaf 2)
export LESS_TERMCAP_ZH
LESS_TERMCAP_ZR=$(tput ritm; tput op)
export LESS_TERMCAP_ZR

# underline
LESS_TERMCAP_us=$(tput smul; tput setaf 2)
export LESS_TERMCAP_us
LESS_TERMCAP_ue=$(tput rmul; tput op)
export LESS_TERMCAP_ue

# Saner ls colors on macOS
if [ "$(uname -s)" = 'Darwin' ]; then
    LSCOLORS="ExGxFxdaCxDaDahbadacec"
    export LSCOLORS
fi

# Aliases
[ -s ~/.bash_aliases ] && source ~/.bash_aliases

# Bash Completion
if [ -z "$INSIDE_EMACS" ] || [ "$INSIDE_EMACS" = "vterm" ] || [ "$EMACS_BASH_COMPLETE" = "t" ] && ! shopt -oq posix; then
    if [ -s /usr/share/bash-completion/bash_completion ]; then # Debian
        source /usr/share/bash-completion/bash_completion
    elif [ -s /etc/bash_completion ]; then
        source /etc/bash_completion
    elif [ -s /opt/local/etc/bash_completion ]; then # MacPorts
        source /opt/local/etc/bash_completion
    elif [ -s /usr/local/etc/bash_completion ]; then # Homebrew
        source /usr/local/etc/bash_completion
    fi

    # Emacs vterm support
    if [ -s "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh" ]; then
        source "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh"
    fi

    # Node
    [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/bash_completion"

    # Go
    if [ -x "$(type -P gocomplete)" ]; then
        complete -C gocomplete go
    fi

    # sdkman!
    [ "$(type -t sdk)" = 'function' ] && eval "$(sdk completion bash)"

    # gcloud
    if [ -s "$HOME/.google-cloud-sdk/completion.bash.inc" ]; then
        source "$HOME/.google-cloud-sdk/completion.bash.inc";
    fi

    # aws
    [ -x "$(type -P aws_completer)" ] && complete -C "$(type -p aws_completer)" aws

    # pyenv
    if [ -s "$PYENV_ROOT/completions/pyenv.bash" ]; then
        source "$PYENV_ROOT/completions/pyenv.bash"
    fi

    # git-subrepo
    if [ -s "$GIT_SUBREPO_ROOT/share/completion.bash" ]; then
        source "$GIT_SUBREPO_ROOT/share/completion.bash"
    fi

    # CircleCI
    [ -x "$(type -P circleci)" ] && eval "$(circleci completion bash)"
fi

# Direnv
if [ -x "$(type -P direnv)" ]; then
    eval "$(direnv hook bash)"
fi

# pyenv
if [ "$(type -t pyenv)" != function ]; then
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

# Prompt
if [ -x "$(type -P starship)" ]; then
    eval "$(starship init bash)"
else
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

# Better search
if [ -d "$HOME/.fzf" ]; then
    PATH="$HOME/.fzf/bin:$PATH"
    MANPATH="$HOME/.fzf/man:$MANPATH"
    FZF_PREFIX="$HOME/.fzf/shell"
else
    case "$(uname -s)" in
        Darwin)
            FZF_PREFIX='/opt/local/share/fzf/shell'
            ;;
        Linux)
            FZF_PREFIX='/usr/share/doc/fzf/examples'
            ;;
        *)
            ;;
    esac
fi

if [ -x "$(type -P fzf)" ]; then
    FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=fg:-1,bg:-1,fg+:15:regular,bg+:0\
                                        --color=hl:11,hl+:13:bold\
                                        --color=prompt:10:bold,pointer:15,marker:11,info:4,spinner:5:bold"
    export FZF_DEFAULT_OPTS

    FZF_DEFAULT_COMMAND='if [ -x "$(type -P git)" ] && [ -n "$(git rev-parse --show-toplevel)" ]; then
                             git ls-tree -r --name-only HEAD
                         elif [ -x "$(type -P fd)" ]; then
                             fd -uu --no-ignore-parent --type f
                         elif [ -x "$(type -P fdfind)" ]; then
                             fdfind -uu --no-ignore-parent --type f
                         elif [ -x "$(type -P rg)" ]; then
                             rg --no-ignore --no-hidden --files || find .
                         fi'
    export FZF_DEFAULT_COMMAND

    FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_CTRL_T_COMMAND

    FZF_ALT_C_COMMAND='if [ -x "$(type -P git)" ] && [ -n "$(git rev-parse --show-toplevel)" ]; then
                           git ls-tree -d -r --name-only HEAD
                       elif [ -x "$(type -P fd)" ]; then
                           fd -uu --no-ignore-parent --type d
                       elif [ -x "$(type -P fdfind)" ]; then
                           fdfind -uu --no-ignore-parent --type d
                       fi'
    export FZF_ALT_C_COMMAND

    if [ -r "$FZF_PREFIX/key-bindings.bash" ]; then
        source "$FZF_PREFIX/key-bindings.bash"
    fi

    if [ -r "$FZF_PREFIX/completion.bash" ]; then
        source "$FZF_PREFIX/completion.bash"
    fi
fi

# Magic fix for command not found error lifted from
# https://github.com/Bash-it/bash-it/blob/ab3b8b8f2204181c46ff9dff7d3c401447200420/plugins/available/osx.plugin.bash
if [[ $OSTYPE == 'darwin'* ]]; then
    if type update_terminal_cwd > /dev/null 2>&1 ; then
        if ! [[ $PROMPT_COMMAND =~ (^|;)update_terminal_cwd($|;) ]] ; then
            PROMPT_COMMAND="${PROMPT_COMMAND%;};update_terminal_cwd"
            declared="$(declare -p PROMPT_COMMAND)"
            [[ "$declared" =~ \ -[aAilrtu]*x[aAilrtu]*\  ]] 2>/dev/null
            [[ $? -eq 0 ]] && export PROMPT_COMMAND
        fi
    fi
fi

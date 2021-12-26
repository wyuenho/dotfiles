# -*- mode: sh; -*-

if [ -x /usr/bin/dircolors ]; then # Debian
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls="ls -Fh --color=auto"
else
    alias ls="ls -FhG" # BSD
fi

if [ "$(uname)" = 'Darwin' ]; then
    alias ll='ls -AFl'
else
    alias ll='ls -@AFOel'
fi
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

if [ -x "$(type -P git)" ]; then
    alias git='git --config-env=user.email=EMAIL'
fi

if [ -x "$(type -P fzf)" ]; then
    if [ -x "$(type -P jq)" ]; then
        jiq() {
            if [ ! $1 = *."json" ]; then
                echo "$1 is not a JSON document"
                return 1
            fi
            echo '' | fzf --print-query --preview "cat $1 | jq {q}"
        }
    fi

    fe() {
        local IFS=$'\n'
        local files
        files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
        [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
    }
fi

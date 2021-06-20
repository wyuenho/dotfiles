# Aliases
if [ -x /usr/bin/dircolors ]; then # Debian
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls="ls -Fh --color=auto"
else
    alias ls="ls -FhG" # BSD
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

if [ -x "$(command -v git)" ]; then
    alias git='git --config-env=user.email=EMAIL'
fi

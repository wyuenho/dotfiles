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

# Bash Completion
if [ -f /opt/local/etc/bash_completion ]; then
    # MacPorts
    source /opt/local/etc/bash_completion
else if [ -f /usr/local/share/bash-completion/bash_completion ]; then
         # Homebrew
         source /usr/local/share/bash-completion/bash_completion
     fi
fi

bind 'set match-hidden-files off'

# Prompt
if [ -f $HOME/.scm-prompt.sh ]; then
    source "$HOME/.scm-prompt.sh"
    PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\[\e[01;35m\]$(_scm_prompt)\[\e[0m\]\n\$ '
else
    PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '
fi

# Bash command search auto-completion
export HH_CONFIG=hicolor         # get more colors
shopt -s histappend              # append new history items to .bash_history
export HISTCONTROL=ignorespace   # leading space hides commands from history
export HISTFILESIZE=10000        # increase history file size (default is 500)
export HISTSIZE=${HISTFILESIZE}  # increase history size (default is 500)
export PROMPT_COMMAND="history -a; history -n;"   # mem/file sync
# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* && "$(type -fp hh)" ]]; then bind '"\C-r": "\C-a hh -- \C-j"'; fi

# Paths
export PATH
PATH=".:$HOME/packages/sshuttle:$HOME/packages/bin:/opt/local/bin:/opt/local/sbin:$PATH:/usr/local/sbin"

export EDITOR
EDITOR="emacs -Q -mm"

export MBOX
MBOX="$HOME/.mail/mbox"

# C/C++
export CC
CC="cc"
export CXX
CXX="c++"

# Java
export JAVA_HOME
JAVA_HOME=$(/usr/libexec/java_home -v 9)
PATH="$HOME/packages/elasticsearch/bin:$PATH"
PATH="$HOME/packages/kibana/bin:$PATH"

# Python
export PYTHONSTARTUP
PYTHONSTARTUP="$HOME/.pythonrc"
if [ "$(type -fp pip)" ]; then eval "$(pip completion -b)"; fi
export PYENV_ROOT
PYENV_ROOT="$HOME/.pyenv"
PATH="$HOME/.pyenv/bin:$HOME/Library/Python/2.7/bin:$PATH"
if [ "$(type -fp pyenv)" ]; then eval "$(pyenv init -)"; fi
if [ "$(type -fp virtualenv)" ]; then eval "$(pyenv virtualenv-init -)"; fi

# Node
export NVM_DIR
NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -r "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"
if [ "$(type -fp grunt)" ]; then eval "$(grunt --completion=bash)"; fi
if [ "$(type -fp gulp)" ]; then eval "$(gulp --completion=bash)"; fi
if [ "$(type -fp npm)" ]; then eval "$(npm completion)"; fi
PATH="$HOME/.yarn/bin:$PATH"

# Ruby
PATH="$HOME/.rbenv/bin:$PATH"
if [ "$(type -fp rbenv)" ]; then eval "$(rbenv init -)"; fi

# Go
if [[ -s "$HOME/.gvm/scripts/gvm" ]]; then
    source "$HOME/.gvm/scripts/gvm"
    export GOROOT_BOOTSTRAP
    GOROOT_BOOTSTRAP=$(go env GOROOT)
fi

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

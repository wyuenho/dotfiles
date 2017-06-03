PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '

export PATH
PATH=".:$HOME/packages/m-cli:$HOME/packages/sshuttle:$HOME/packages/bin:/opt/local/bin:/opt/local/sbin:$PATH"

export MANPATH
MANPATH="/opt/local/share/man:$MANPATH"

export EDITOR
EDITOR="emacs -Q -mm"

export MBOX
MBOX="$HOME/.mail/mbox"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

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

if [ -f /opt/local/etc/bash_completion ]; then
    source /opt/local/etc/bash_completion
fi

bind 'set match-hidden-files off'

# C/C++
export CC
CC="cc"
export CXX
CXX="c++"

# Java
export JAVA_HOME
JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
PATH="$HOME/packages/elasticsearch/bin:$PATH"
PATH="$HOME/packages/kibana/bin:$PATH"

# Python
export PYTHONSTARTUP
PYTHONSTARTUP="$HOME/.pythonrc"
PATH="$HOME/Library/Python/2.7/bin:$PATH"
export PYENV_ROOT
PYENV_ROOT="$HOME/.pyenv"
PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
if [ "$(type -fp pip)" ]; then eval "$(pip completion -b)"; fi

# Node
export NVM_DIR
NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm
[ -r "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"
if [ "$(type -fp grunt)" ]; then eval "$(grunt --completion=bash)"; fi
if [ "$(type -fp gulp)" ]; then eval "$(gulp --completion=bash)"; fi
PATH="$HOME/.yarn/bin:$PATH"
if [ "$(type -fp npm)" ]; then eval "$(npm completion)"; fi

# Ruby
PATH="$HOME/.rbenv/bin:$PATH"
if [ "$(type -fp rbenv)" ]; then eval "$(rbenv init -)"; fi

# Go
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export GOROOT_BOOTSTRAP
GOROOT_BOOTSTRAP=$(go env GOROOT)

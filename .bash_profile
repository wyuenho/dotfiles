PATH="$HOME/packages/sshuttle:$HOME/packages/bin:/opt/local/bin:/opt/local/sbin:$PATH:/usr/local/sbin"

# C/C++
export CC
CC="cc"
export CXX
CXX="c++"

# Java
export JAVA_HOME
JAVA_HOME=$(/usr/libexec/java_home -v 9)

# Python
export PYTHONSTARTUP
PYTHONSTARTUP="$HOME/.pythonrc"
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
PATH="$HOME/.yarn/bin:$PATH"

# Ruby
PATH="$HOME/.rbenv/bin:$PATH"
if [ "$(type -fp rbenv)" ]; then eval "$(rbenv init -)"; fi

# Go
if [[ -s "$HOME/.gvm/scripts/gvm" ]]; then
    source "$HOME/.gvm/scripts/gvm"
    source "$HOME/.gvm/scripts/completion"
    export GOROOT_BOOTSTRAP
    GOROOT_BOOTSTRAP=$(go env GOROOT)
fi

# Rust
PATH="$HOME/.cargo/bin:$PATH"

# PWD
PATH=".:$PATH"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

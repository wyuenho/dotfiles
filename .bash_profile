# Homebrew
PATH="/usr/local/sbin:$PATH"

# MacPorts
PATH="/opt/local/bin:/opt/local/sbin:$PATH"
MANPATH="/opt/local/share/man:$MANPATH"

# My Own things
PATH="$HOME/packages/bin:$PATH"

# C/C++
export CC
CC="cc"
export CXX
CXX="c++"

# Java
export JAVA_HOME
JAVA_HOME=$(/usr/libexec/java_home -v 11)

# Python
export PYTHONSTARTUP
PYTHONSTARTUP="$HOME/.pythonrc"
export PYENV_ROOT
PYENV_ROOT="$HOME/.pyenv"
PATH="$HOME/.pyenv/bin:$HOME/Library/Python/2.7/bin:$PATH"
if [ "$(type -fp pyenv)" ]; then
    eval "$(pyenv init -)";
    if pyenv commands | grep -q virtualenv; then
        eval "$(pyenv virtualenv-init -)";
    fi
fi

# Node
export NVM_DIR
NVM_DIR="$HOME/.nvm"
[ -f "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -f "$HOME/.avn/bin/avn.sh" ] && source "$HOME/.avn/bin/avn.sh"
PATH="$HOME/.yarn/bin:$PATH"

# Ruby
PATH="$HOME/.rbenv/bin:$PATH"
if [ "$(type -fp rbenv)" ]; then eval "$(rbenv init -)"; fi

# Go
if [ -f "$HOME/.gvm/scripts/gvm" ]; then
    source "$HOME/.gvm/scripts/gvm"
    export GOROOT_BOOTSTRAP
    GOROOT_BOOTSTRAP=$(go env GOROOT)
    export GO111MODULE
    GO111MODULE="on"
fi

# Rust
PATH="$HOME/.cargo/bin:$PATH"

# OPAM configuration
if [ -f "$HOME/.opam/opam-init/init.sh" ]; then
    source "$HOME/.opam/opam-init/init.sh"
fi

# Haskell
PATH="$HOME/Library/Haskell/bin:$PATH"

# Google Cloud SDK
export CLOUDSDK_PYTHON
CLOUDSDK_PYTHON="python"
if [ -f "$HOME/.google-cloud-sdk/path.bash.inc" ]; then
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# PWD
PATH=".:$PATH"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

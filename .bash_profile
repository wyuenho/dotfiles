# ~/.bash_profile: executed by the command interpreter for login shells
# ~/.profile is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

# Homebrew
PATH="/usr/local/sbin:$PATH"

# MacPorts
if [ -d /opt/local ]; then
    PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    MANPATH="/opt/local/share/man:$MANPATH"
fi

# My Own things
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -x "$(command -v lesspipe.sh)" ]; then
    export LESSOPEN
    LESSOPEN='| /opt/local/bin/lesspipe.sh %s'
fi

# C/C++
export CC
CC="cc"
export CXX
CXX="c++"

# Java
if /usr/libexec/java_home -v 11 > /dev/null 2>&1; then
    export JAVA_HOME
    JAVA_HOME=$(/usr/libexec/java_home -v 11)
fi

# Python
export PYTHONSTARTUP
PYTHONSTARTUP="$HOME/.pythonrc"

if [ -d "$HOME/Library/Python/2.7/bin" ]; then
    PATH="$HOME/Library/Python/2.7/bin:$PATH"
fi

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT
    PYENV_ROOT="$HOME/.pyenv"
    PATH="$HOME/.pyenv/bin:$PATH"
    if [ -x "$(command -v pyenv)" ]; then
        eval "$(pyenv init -)";
        if pyenv commands | grep -q virtualenv; then
            eval "$(pyenv virtualenv-init -)";
        fi
    fi
fi

# Node
export NVM_DIR
NVM_DIR="$HOME/.nvm"
if [ -f "$NVM_DIR/nvm.sh" ]; then
    source "$NVM_DIR/nvm.sh"
fi

if [ -f "$HOME/.avn/bin/avn.sh" ]; then
    source "$HOME/.avn/bin/avn.sh"
fi

if [ -d "$HOME/.yarn/bin" ]; then
    PATH="$HOME/.yarn/bin:$PATH"
fi

# Ruby
if [ -d "$HOME/.rbenv/bin" ]; then
    PATH="$HOME/.rbenv/bin:$PATH"
    if [ -x "$(command -v rbenv)" ]; then
        eval "$(rbenv init -)";
    fi
fi

# Go
if [ -f "$HOME/.gvm/scripts/gvm" ]; then
    source "$HOME/.gvm/scripts/gvm"
    export GOROOT_BOOTSTRAP
    GOROOT_BOOTSTRAP=$(go env GOROOT)
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# OPAM configuration
if [ -f "$HOME/.opam/opam-init/init.sh" ]; then
    source "$HOME/.opam/opam-init/init.sh"
fi

# Haskell
if [ -f "$HOME/Library/Haskell/bin" ]; then
    PATH="$HOME/Library/Haskell/bin:$PATH"
fi

# Google Cloud SDK
export CLOUDSDK_PYTHON
CLOUDSDK_PYTHON="python"
if [ -f "$HOME/.google-cloud-sdk/path.bash.inc" ]; then
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# Netlify's Git Credential Helper.
if [ -f "$HOME/.netlify/helper/path.bash.inc" ]; then
    source "$HOME/.netlify/helper/path.bash.inc"
fi

# PWD
PATH=".:$PATH"

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

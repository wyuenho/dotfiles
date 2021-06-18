# ~/.bash_profile: executed by the command interpreter for login shells
# ~/.profile is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

# adding an appropriate DISPLAY variable for use with MacPorts.
export DISPLAY=:0

if [ -z "$MANPATH" ]; then # MacOS
    # Must be set first for /usr/libexec/path_helper to modify
    export MANPATH
    MANPATH=""

    # Run again to set MANPATH
    if [ -x /usr/libexec/path_helper ]; then
        eval "$(/usr/libexec/path_helper -s)"
    fi
fi

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME
XDG_CONFIG_HOME="$HOME/.config"

export XDG_CACHE_HOME
XDG_CACHE_HOME="$HOME/.cache"

export XDG_DATA_HOME
XDG_DATA_HOME="$HOME/.local/share"

# Xcode CLI Tools
if [ -d "/Library/Developer/CommandLineTools/usr/share/man" ]; then
    MANPATH="/Library/Developer/CommandLineTools/usr/share/man:$MANPATH"
fi

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

# pipx
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# git-subrepo
if [ -d "$HOME/.local/git-subrepo" ]; then
    export GIT_SUBREPO_ROOT
    GIT_SUBREPO_ROOT="$HOME/.local/git-subrepo"
    PATH="$GIT_SUBREPO_ROOT/lib:$PATH"
    MANPATH="$GIT_SUBREPO_ROOT/man:$MANPATH"
fi

# Lesspipe
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
    export KEYTOOL
    KEYTOOL="$JAVA_HOME/jre/bin"
fi

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    export SDKMAN_DIR
    SDKMAN_DIR="$HOME/.sdkman"
    source "$SDKMAN_DIR/bin/sdkman-init.sh"
    unset KEYTOOL
fi

# Python
if [ -f "$HOME/.pythonrc" ]; then
    export PYTHONSTARTUP
    PYTHONSTARTUP="$HOME/.pythonrc"
fi

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT
    PYENV_ROOT="$HOME/.pyenv"
    PATH="$PYENV_ROOT/bin:$PATH"
    if [ -x "$(command -v pyenv)" ]; then
        eval "$(pyenv init --path)";
        if pyenv commands | grep -q virtualenv; then
            eval "$(pyenv virtualenv-init -)";
        fi
    fi
fi

# Node
if [ -z "${XDG_CONFIG_HOME-}" ]; then
    NVM_DIR="$HOME/.nvm"
else
    NVM_DIR="${XDG_CONFIG_HOME}/nvm"
fi

if [ -d "$NVM_DIR" ]; then
    export NVM_DIR
    source "$NVM_DIR/nvm.sh"
fi

if [ -f "$HOME/.avn/bin/avn.sh" ]; then
    source "$HOME/.avn/bin/avn.sh"
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
if [ -f "$HOME/.ghcup/env" ]; then
    source "$HOME/.ghcup/env" # ghcup-env
fi

# Google Cloud SDK
if [ -f "$HOME/.google-cloud-sdk/path.bash.inc" ]; then
    export CLOUDSDK_PYTHON
    CLOUDSDK_PYTHON="python"
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# PWD
PATH=".:$PATH"

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

# ~/.bash_profile: executed by the command interpreter for login shells
# ~/.profile is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

# adding an appropriate DISPLAY variable for use with MacPorts.
DISPLAY=:0
export DISPLAY

if [ -z "$MANPATH" ]; then # MacOS
    # Must be set first for /usr/libexec/path_helper to modify
    MANPATH=""
    export MANPATH

    # Run again to set MANPATH
    if [ -x /usr/libexec/path_helper ]; then
        eval "$(/usr/libexec/path_helper -s)"
    fi
fi

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_HOME

XDG_CACHE_HOME="$HOME/.cache"
export XDG_CACHE_HOME

XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_HOME

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
    GIT_SUBREPO_ROOT="$HOME/.local/git-subrepo"
    export GIT_SUBREPO_ROOT
    PATH="$GIT_SUBREPO_ROOT/lib:$PATH"
    MANPATH="$GIT_SUBREPO_ROOT/man:$MANPATH"
fi

# C/C++
CC="cc"
export CC
CXX="c++"
export CXX

# Java
if /usr/libexec/java_home -v 11 > /dev/null 2>&1; then
    JAVA_HOME=$(/usr/libexec/java_home -v 11)
    export JAVA_HOME
    KEYTOOL="$JAVA_HOME/jre/bin"
    export KEYTOOL
fi

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    SDKMAN_DIR="$HOME/.sdkman"
    export SDKMAN_DIR
    source "$SDKMAN_DIR/bin/sdkman-init.sh"
    unset KEYTOOL
fi

# Python
if [ -f "$HOME/.pythonrc" ]; then
    PYTHONSTARTUP="$HOME/.pythonrc"
    export PYTHONSTARTUP
fi

if [ -d "$HOME/.pyenv" ]; then
    PYENV_ROOT="$HOME/.pyenv"
    export PYENV_ROOT
    PATH="$PYENV_ROOT/bin:$PATH"
    if [ -x "$(type -P pyenv)" ]; then
        eval "$(pyenv init --path)";
        if pyenv commands | grep -q virtualenv; then
            if [ -x "$(type -P starship)" ]; then
                export PATH="$PYENV_ROOT/plugins/pyenv-virtualenv/shims:${PATH}";
                export PYENV_VIRTUALENV_INIT=1;
            else
                eval "$(pyenv virtualenv-init -)";
            fi
        fi
    fi
fi

# Node
if [ -d "$HOME/.volta" ]; then
    VOLTA_HOME="$HOME/.volta"
    export VOLTA_HOME

    PATH="$VOLTA_HOME/bin:$PATH"
fi

# Ruby
if [ -d "$HOME/.rbenv/bin" ]; then
    PATH="$HOME/.rbenv/bin:$PATH"
    if [ -x "$(type -P rbenv)" ]; then
        eval "$(rbenv init -)";
    fi
fi

# Go
if [ -x "$(type -P go)" ]; then
    GOPATH="$HOME/Documents/workspace/go"
    PATH="$GOPATH/bin:$PATH"
    export GOPATH
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
    CLOUDSDK_PYTHON="python"
    export CLOUDSDK_PYTHON
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# PWD
PATH="::$PATH"

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

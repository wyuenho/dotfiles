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

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Homebrew
if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# MacPorts
if [ -d /opt/local ]; then
    PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    MANPATH="/opt/local/share/man:$MANPATH"
fi

# Cross-compiler toolchains
if [ -d /Volumes/toolchains ]; then
    for triple in /Volumes/toolchains/*/bin; do
        PATH="$triple:$PATH"
    done
fi

# git-subrepo
if [ -d "$HOME/.local/git-subrepo" ]; then
    GIT_SUBREPO_ROOT="$HOME/.local/git-subrepo"
    export GIT_SUBREPO_ROOT
    PATH="$GIT_SUBREPO_ROOT/lib:$PATH"
    MANPATH="$GIT_SUBREPO_ROOT/man:$MANPATH"
fi

# Java
if /usr/libexec/java_home -v 11 > /dev/null 2>&1; then
    JAVA_HOME=$(/usr/libexec/java_home -v 11)
    export JAVA_HOME
    KEYTOOL="$JAVA_HOME/jre/bin"
    export KEYTOOL
fi

if [ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
    SDKMAN_DIR="$HOME/.sdkman"
    export SDKMAN_DIR
    source "$SDKMAN_DIR/bin/sdkman-init.sh"
    unset KEYTOOL
fi

# Python
if [ -s "$HOME/.pythonrc" ]; then
    PYTHONSTARTUP="$HOME/.pythonrc"
    export PYTHONSTARTUP
fi

# Ruby
if [ -d "$HOME/.rbenv/bin" ]; then
    PATH="$HOME/.rbenv/bin:$PATH"
    if [ -x "$(type -P rbenv)" ]; then
        eval "$(rbenv init -)"
    fi
fi

# Go
if [ -x "$(type -P go)" ]; then
    GOBIN=$(go env GOBIN)
    GOBIN="${GOBIN:-$(go env GOPATH)/bin}"
    PATH="$GOBIN:$PATH"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# OPAM configuration
if [ -s "$HOME/.opam/opam-init/init.sh" ]; then
    source "$HOME/.opam/opam-init/init.sh"
fi

# Haskell
if [ -s "$HOME/.ghcup/env" ]; then
    source "$HOME/.ghcup/env" # ghcup-env
fi

# Google Cloud SDK
if [ -s "$HOME/.google-cloud-sdk/path.bash.inc" ]; then
    CLOUDSDK_PYTHON="$(which python)"
    export CLOUDSDK_PYTHON
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# k8s
if [ -d "$HOME/.rd/bin" ]; then
    PATH="$HOME/.rd/bin:$PATH"
fi

if [ -d "$HOME"/.krew/bin ]; then
    PATH="$HOME/.krew/bin:$PATH"
fi

# PWD
PATH=":$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup=$("$HOME/miniconda3/bin/conda" 'shell.bash' 'hook' 2> /dev/null)
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

if [ -n "$BASH_VERSION" ] && [ -s "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
fi

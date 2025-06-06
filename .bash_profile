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

# Node
if [ -x "$(type -P fnm)" ]; then
    eval "$(fnm env --version-file-strategy=recursive --corepack-enabled)"
fi

# Python

# PATH="$HOME/.pixi/bin:$PATH"

# PATH="$PATH:$HOME/Library/Application Support/hatch/pythons/3.12/python/bin"

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

# LM Studio CLI
if [ -d "$HOME/.lmstudio/bin" ]; then
    PATH="$HOME/.lmstudio/bin:$PATH"
fi

# PWD
PATH=":$PATH"

if [ -n "$BASH_VERSION" ] && [ -s "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
fi

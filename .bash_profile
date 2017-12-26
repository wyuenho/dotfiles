# Homebrew
PATH="/usr/local/sbin:$PATH"

# MacPorts
PATH="/opt/local/bin:/opt/local/sbin:$PATH"

# My Own things
PATH="$HOME/packages/bin:$PATH"
PATH="$HOME/packages/sshuuttle:$PATH"

# ImageMagick
export MAGICK_HOME
MAGICK_HOME="$HOME/packages/ImageMagick"
export DYLD_LIBRARY_PATH
DYLD_LIBRARY_PATH="$MAGICK_HOME/lib/"
PATH="$MAGICK_HOME/bin:$PATH"

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
if [ "$(type -fp pyenv)" ]; then
    eval "$(pyenv init -)";
    if pyenv commands | grep -q virtualenv; then
        eval "$(pyenv virtualenv-init -)";
    fi
fi

# Node
export NVM_DIR
NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
PATH="$HOME/.yarn/bin:$PATH"

# Ruby
PATH="$HOME/.rbenv/bin:$PATH"
if [ "$(type -fp rbenv)" ]; then eval "$(rbenv init -)"; fi

# Go
if [ -s "$HOME/.gvm/scripts/gvm" ]; then
    source "$HOME/.gvm/scripts/gvm"
    export GOROOT_BOOTSTRAP
    GOROOT_BOOTSTRAP=$(go env GOROOT)
fi

# Rust
PATH="$HOME/.cargo/bin:$PATH"

# Google Cloud SDK
if [ -f "$HOME/.google-cloud-sdk/path.bash.inc" ]; then
    source "$HOME/.google-cloud-sdk/path.bash.inc"
fi

# PWD
PATH=".:$PATH"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

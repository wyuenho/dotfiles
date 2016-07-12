export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '

export PATH=".:${HOME}/Library/Python/3.5/bin:${HOME}/Library/Python/2.7/bin:$HOME/packages/elasticsearch/bin:$HOME/packages/kibana/bin:${HOME}/packages/sshuttle:${HOME}/packages/bin:/opt/local/bin:/opt/local/sbin:/usr/local/mysql/bin:${PATH}"

export MANPATH="/opt/local/share/man:/usr/local/mysql/man:${MANPATH}"
export TERM="xterm-color"
export EDITOR="emacs -Q -mm"
export CC="cc"
export CXX="c++"
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export MBOX="${HOME}/.mail/mbox"
export PYTHONSTARTUP="${HOME}/.pythonrc"

if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

if [ `which ls` == "/bin/ls" ]; then
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
   . /opt/local/etc/bash_completion
fi

bind 'set match-hidden-files off'

# pip bash completion
if [ $(type -fp pip) ]; then eval "$(pip completion -b)"; fi

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if [ $(type -fp rbenv) ]; then eval "$(rbenv init -)"; fi

# grunt bash completion
#if [ $(type -fp grunt) ]; then eval "$(grunt --completion=bash)"; fi

# GVM
[[ -s "${HOME}/.gvm/scripts/gvm" ]] && source "${HOME}/.gvm/scripts/gvm"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

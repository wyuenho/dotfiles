export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

PS1='\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ '

PATH=".:${HOME}/Library/Python/2.7/bin:${HOME}/.rbenv/bin:${HOME}/packages/pypy/bin:$HOME/packages/elasticsearch/bin:$HOME/packages/kibana/bin:${HOME}/packages/sshuttle:${HOME}/packages/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/mysql/bin:${PATH}"
export PATH

MANPATH="/opt/local/share/man:/usr/local/mysql/man:${MANPATH}"
export MANPATH

TERM="xterm-color"
export TERM
 
EDITOR="emacs -Q -mm"
export EDITOR

CC="clang"
export CC

JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export JAVA_HOME

MBOX="${HOME}/.mail/mbox"
export MBOX

PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONSTARTUP

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
if [ -f `which pip` ]; then eval "$(pip completion -b)"; fi

# rbenv
if [ -f `which rbenv` ]; then eval "$(rbenv init -)"; fi

# grunt bash completion
if [ -f `which grunt` ]; then eval "$(grunt --completion=bash)"; fi

# NVM
[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh # This loads NVM
[[ -r $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion

export NODE_REPL_HISTORY="$HOME/.node_repl_history"

# GVM
[[ -s "${HOME}/.gvm/scripts/gvm" ]] && source "${HOME}/.gvm/scripts/gvm"

# Docker
source ~/.docker-completion.sh
source ~/.docker-compose-completion.sh
source ~/.docker-machine-completion.sh

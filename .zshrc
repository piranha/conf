# -*- mode: sh -*-
# zsh configuration
# (c) 2003-2016 Alexander Solovyov
# alexander AT solovyov.net

export LANG=en_US.UTF-8
export LC_TIME=C
export LC_NUMERIC=C

[[ $TERM = "xterm" ]] && stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

if [ $(uname -s) = "Darwin" ]; then
    # when it's unlimited, it's not. Go figure. :\
    ulimit -S -n 4096
fi

umask 022

export PATH=~/bin:/usr/local/go/bin:/opt/homebrew/sbin:/opt/homebrew/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
test -x "/bin/launchctl" && sudo launchctl setenv PATH $PATH

export PAGER="less"
if [ -x "`whence -c vim`" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi
export LESS="FRQXi"
export PS_FORMAT="user,group,pid,rss,sz,stime,time,cmd"
export PIP_RESPECT_VIRTUALENV=true
#export JAVA_TOOL_OPTIONS='-Djava.awt.headless=true'
export BOOT_JVM_OPTIONS='-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xverify:none -Xmx2g'
export FZF_DEFAULT_OPTS="--ansi --color light --preview 'head -100 {}' --select-1"
export FZF_DEFAULT_COMMAND="fd -t f"
export FZF_CTRL_T_OPTS="$FZF_DEFAULT_OPTS"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export EXA_COLORS="*.md=black:Makefile=black"
export CDPATH=~/dev/work:~/dev/web:~/dev/misc:~/dev
export HOMEBREW_AUTO_UPDATE_SECS=2592000

# local settings can override some settings
if [ -f ~/.zshlocal ]; then
    source ~/.zshlocal
fi

### VCS

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

zstyle ':vcs_info:git*' formats "[%b] "
zstyle ':vcs_info:git*' actionformats "[%b: %a] "

### Prompt

# nbsp allows to have space which will delete everything before itself when
# pasted - so it's possible to copy and paste whole line with prompt and have
# only command left
nbsp=$'\u00A0'
bindkey -s $nbsp '^u'

p_at='%(!.%F{red}%B#%b%f.@)'
p_host='%F{blue}%m%f'
p_path='%F{blue}%~%f'
p_pr='%(?.%F{blue}>%f.%F{red}×%f)'

PS1="$p_at$p_host $p_path$p_pr$nbsp"
unset p_at p_host p_path p_pr nbsp

setopt promptsubst

fpath=(~/.zsh.d /usr/local/share/zsh/site-functions /usr/local/share/zsh-completions $fpath)

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# History
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000
setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt autocd
setopt auto_pushd pushd_ignore_dups pushd_silent pushd_to_home

# other important options
unsetopt extended_glob # it's quite annoying
setopt notify # report the status of backgrounds jobs immediately
setopt completeinword
setopt hash_list_all
#setopt printexitvalue
REPORTTIME=10
watch=(notme root)

# Loading builtins
autoload -U zmv
zmodload -i zsh/deltochar

# ZLE
bindkey -e
bindkey "[2~" transpose-words
bindkey "[3~" delete-char
bindkey "[1~" beginning-of-line
bindkey "[4~" end-of-line
bindkey "[A" up-line-or-history
bindkey "[B" down-line-or-history
bindkey '^[[5D' emacs-backward-word
bindkey '^[[5C' emacs-forward-word
bindkey '\ew' kill-region
bindkey '\ez' delete-to-char
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line

# press meta-e for editing command line in $EDITOR or $VISUAL
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

autoload -U select-word-style
select-word-style normal
# don't contains -_/= - and thus breaks words on them
zstyle ':zle:*' word-chars '*?.[]~&;!#$%^(){}<>'

# M-. selects last word from a line, and M-m allows to iterate words from this
# line
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word

# Complete from history with M-/
zstyle ':completion:history-words:*' list no
zstyle ':completion:history-words:*' menu yes
zstyle ':completion:history-words:*' remove-all-dups yes
bindkey "\e/" _history-complete-older

######## Completion #######
autoload -U compinit

if [ $UID -eq 0 ]; then
    compinit -i -d ~/.zrootcompdump
else
    compinit -i
fi

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zcompcache
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' max-errors 1 numeric

# Completing process IDs with menu selection
zstyle ':completion:*:processes' command 'ps -ax'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
compdef pkill=kill
compdef pkill=killall

# cd will never select the parent directory (e.g.: cd ../<TAB>)
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle :compinstall filename '.zshrc'
zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
compctl -k hosts ssh-copy-id

compctl -o wget make man rpm iptables
compctl -j -P "%" kill
compctl -g '*.rar' + -g '*(-/)' rar unrar
compctl -g '*(-*)' + -g '*(-/)' strip
compctl -g '*(-/) .*(-/)' cd
compctl -g '(^(*.o|*.class|*.jar|*.gz|*.gif|*.a|*.Z|*.bz2))' + -g '.*' less vim
compctl -g '(^(*.o|*.class|*.jar|*.gif|*.a))' + -g '.*' most
compctl -g '*.fb2 *.fb2.zip' FBReader

function _say_voices {
    reply=(${(f)"$(say -v '?' | awk -F '  ' '{print $1}')"})
}
compctl -x 'c[-1,-v]' -M 'm:{a-zA-Z}={A-Za-z}' -K _say_voices -- say

##### end of completion #####

## xterm header
## \e]0; will set window's and tab's header
## \e]1; will set tab's header
case $TERM in
xterm*|rxvt*)
    title_precmd () {
        print -Pn "\e]0;@%m %~\a"
        print -Pn "\e]1;%1d\a"
    }
    title_preexec () {
        print -Pn "\e]1;%1d - $1\a"
    }
;;
screen)
    title_precmd () {
        print -Pn "\ek@%m %~\a"
    }
    title_preexec () {
        print -Pn "\ek%1d - $1\a"
    }
;;
esac

#### precmd

precmd_functions=(title_precmd)
preexec_functions=(title_preexec)

### cdr

ZSH_CDR_DIR=${XDG_CACHE_HOME:-$HOME/.cache}/zsh-cdr
mkdir -p $ZSH_CDR_DIR

autoload -Uz chpwd_recent_dirs cdr
autoload -U add-zsh-hook

add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file $ZSH_CDR_DIR/recent-dirs
zstyle ':chpwd:*' recent-dirs-max 1000
# fall through to cd
zstyle ':chpwd:*' recent-dirs-default yes

#############        ALIASES         ###############
# Nocorrect
#alias mv="nocorrect mv"
#alias cp="nocorrect cp"
alias mkdir="nocorrect mkdir"

alias make="env -u CDPATH make"

## GNU Find
if [ `uname` != "Linux" -a -x "`whence -c gfind`" ]; then
    alias find="noglob gfind"
else
    alias find="noglob find"
fi

## Editor
if [ -x "`whence -c emacsclient`" ]; then
    function e() {
        if [ -z $1 ]
        then
            xargs emacsclient --no-wait $LINE
        else
            emacsclient --no-wait $1
        fi
    }
    alias ec="emacsclient -t"
    export ALTERNATE_EDITOR="vim"
    export EDITOR=emacsclient
else
    alias e=$EDITOR
fi

if [ -x "`whence -c rlwrap`" ]; then
    alias nc='rlwrap nc'
fi

# ls
if [ $(uname) = "Linux" ]; then
    alias ls="/bin/ls --color"
elif [ -x "$(whence -c exa)" ]; then
    alias ls=exa
elif [ -x "$(whence -c gls)" ]; then
    alias ls="gls --color"
else
    alias ls="/bin/ls -G"
fi

if [ -x "$(whence -c exa)" ]; then
    alias ll="exa -l --git"
    alias lt="exa -lm"
    alias la="exa -la"
else
    alias ll="ls -lh"
    alias lt="ls -lt"
    alias la="ls -lA"
fi
alias lsd="ls -ld *(-/DN)"
alias lsa="ls -ld .*"

function l() {
    if [ $# -gt 0 -a -d $1 ]; then
        ls $@
    else
        $PAGER $@
    fi
}

# Other
alias m="mutt"
alias rm="rm -f"
#alias mc="mc -acx"
#alias l=$PAGER
alias g="egrep -i --color"
alias h="head"
alias t="tail -f"
alias p="ping"
alias pp="prettyping --nolegend"
alias df="df -h"
alias myapg="apg -a 1 -n 8 -x 9 -M NCL -E l1iI0Oo"
alias rezsh="source ~/.zshrc"
alias s="mdfind -name"
alias -g N='2>&1'
alias -g X='| xargs '
alias $=''

alias ho="sudo vim /etc/hosts"
alias pc="rsync -P"
alias sudo="sudo " # this carries aliases to sudo calls
alias youtube-dl='noglob youtube-dl'
alias wget='noglob wget'
alias curl='noglob curl'
alias rg='noglob rg'

function bdiff() { hg diff -r "ancestor('$1', master)" -r "$1" $2 $3 $4 }

function gkill() { awk '{print $2}' | xargs kill ${@} }

alias pg="pgrep -lf"
function pgk() { pgrep -f $1 | xargs kill }
alias -g B='$(git symbolic-ref HEAD)'
alias master="git checkout master"

# find file by name and open it in emacs
function fe() {
    if [ -z "$1" ]; then
        fd -t f | fzf --print0 | xargs -0 emacsclient --no-wait
    else
        fd -t f "$1" | fzf --print0 | xargs -0 emacsclient --no-wait
    fi
}

# find file by content, then filter it by name and open it in emacs
function ge() {
    rg -l "$1" 2>/dev/null | fzf --print0 | xargs -0 emacsclient --no-wait
}

# find file by name and switch to directory containing that file
function cfd() {
   local file
   local dir
   file=$(fzf -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# search for a git commit in git messages with fzf
function gits() {
  LESS="RQXi"
  local out sha q
  while out=$(
      git log --decorate=short --graph --oneline --color=always |
      fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
    q=$(head -1 <<< "$out")
    while read sha; do
      [ -n "$sha" ] && git show --color=always $sha | less
    done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
  done
}


### repo dir jumper

function gro() {
    cd $(git rev-parse --show-toplevel)
}

function gd() {
    cd $(fd --type f --hidden --follow --exclude .git |
             fzf --preview "ls -Ap {}" --print0 |
             xargs -0 dirname)
}

alias gg="gro && gd"

### /repo dir jumper


function makegif() {
    convert -delay 1x25 *.png -ordered-dither o8x8,9 -coalesce -layers OptimizeTransparency +map -crop 480x270+0+45 +repage animation.gif
}

function mv() {
    if [ $# -lt 2 ]; then
        /bin/mv $1 .
    else
        /bin/mv "$@"
    fi
}

function rtun() {
    if [ -z $1 ]; then
        echo "Usage: rtun PORT [DESTPORT]"
        echo "Setup tunnel from remote host to local; show off local work"
    else
        DPORT=${2:-$1}
        echo "sirius.solovyov.net:$DPORT"
        ssh -N -R "*:${DPORT}:localhost:${1}" sirius.solovyov.net
    fi
}

function workon() {
    source /usr/local/bin/virtualenvwrapper.sh
    workon $@
}

function mkvirtualenv() {
    source /usr/local/bin/virtualenvwrapper.sh
    mkvirtualenv $@
}

function log() {
    echo >> ~/Documents/kb/${*:-log}
    date >> ~/Documents/kb/${*:-log}
    cat >> ~/Documents/kb/${*:-log}
}

function mkcd() {
    if [ ! -z "$1" ]
    then
        mkdir -p "$1"
        cd "$1"
    fi
}

function Q() {
    psql service="$1" "${@:2}"
}

umedit() { [ -z "$1" ] && exit 1; mkdir -p ~/Documents/kb; vim ~/Documents/kb/"$1.md" }
um() { pandoc -s -t man ~/Documents/kb/"$1.md" | tbl | groff -Wall -mtty-char -man -Tascii -c | less -R }

alias dummy_email='python -m smtpd -n -c DebuggingServer localhost:1026'
alias nowrap="tput rmam"
alias wrap="tput smam"

alias magit="emacsclient -n -e '(progn (magit-status) (delete-other-windows))'"

# for emacs' tramp
[[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ ' && unalias ls

function _POST { curl -n -H 'Content-Type: application/json' -XPOST "$@" }
alias POST='noglob _POST'
function _PUT { curl -n -H 'Content-Type: application/json' -XPUT "$@" }
alias PUT='noglob _PUT'
function _GET { curl -n -H 'Content-Type: application/json' -XGET "$@" }
alias GET='noglob _GET'
function _DELETE { curl -n -H 'Content-Type: application/json' -XDELETE "$@" }
alias DELETE='noglob _DELETE'

function _CH { echo "$1" | curl -n -XPOST 'http://ch01.mk.prod:8123/?send_progress_in_http_headers=1&default_format=TabSeparatedWithNames' --data-binary @- "${@:2:100}" }
alias CH='noglob _CH'

alias curlie='noglob curlie -n'

alias -g UA="-H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0'"

# -*- mode: sh -*-
# zsh configuration
# (c) 2003-2011 Alexander Solovyov
# piranha AT piranha.org.ua
#
# Thanks to:
# tldp.org
# Alexander Zayats
# "XAKEP" journal

export LANG=en_US.UTF-8
export LC_TIME=C
export LC_NUMERIC=C

[[ $TERM = "xterm" ]] && stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

umask 022

export PATH=~/bin:~/.cabal/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

export PAGER="less"
if [ -x "`whence -c vim`" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi
export BROWSER="links"
export LESS="FRQXi"
export PS_FORMAT="user,group,pid,rss,sz,stime,time,cmd"
export PIP_RESPECT_VIRTUALENV=true
export WORKON_HOME=~/.virtualenvs

# local settings can override some settings
if [ -f ~/.zshlocal ]; then
    source ~/.zshlocal
fi

# Prompt setup (c) smax 2002, adapted for zsh (c) piranha 2004
# 0-black, 1-red, 2-green, 3-yellow, 4-blue, 5-magenta 6-cyan, 7-white
Cr() { echo '%{\033[3'$1'm%}'; }
hc=`Cr 6`; wc=`Cr 3`; tc=`Cr 7`; w=`Cr 7`; n=`Cr 9`; r=`Cr 1`; y=`Cr 6`; gr=`Cr 2`
[ $UID = 0 ] && at=$r%B'#'%b || at='@'
#PS1="$wc%n$at$hc%m $err$wc%~$w>$n"
# for white background
wc=`Cr 4`
PS1="$wc%n$n$at$wc%m $err$wc%~>$n "
unset n b Cr uc hc wc tc tty at r y gr

fpath=(~/.zsh.d $fpath)

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
setopt auto_pushd
setopt pushd_ignore_dups

# other important options
unsetopt extended_glob # it's quite annoying
setopt notify # report the status of backgrounds jobs immediately
setopt completeinword
setopt hash_list_all
setopt printexitvalue
REPORTTIME=5
watch=(notme root)

# Loading builtins
autoload -U zmv
# load on reference
zmodload -a zsh/zpty zpty

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

######## Completion #######
#hostsmy=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
hosts=(${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*})
#???#zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zcompcache
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*'
zstyle ':completion:*:match:*' original only
zstyle ':completion:*' max-errors 1 numeric

# Completing process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# cd will never select the parent directory (e.g.: cd ../<TAB>)
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle :compinstall filename '.zshrc'

compctl -o wget make man rpm iptables
compctl -k $hosts ssh telnet ping mtr traceroute
compctl -j -P "%" kill
compctl -g '*.gz' + -g '*(-/)' gunzip gzcat
compctl -g '*.rar' + -g '*(-/)' rar unrar
compctl -g '*.bz2' + -g '*(-/)' bunzip2 bzcat
compctl -g '*(-*)' + -g '*(-/)' strip
compctl -g '*.ps *.eps' + -g '*(-/)' gs ghostview psnup psduplex ps2ascii
compctl -g '*.dvi *.pdf *.ps *.ps.gz' + -g '*(-/)' evince epdfview
compctl -g '*.xpm *.xpm.gz' + -g '*(-/)' xpmroot sxpm pixmap xpmtoppm
compctl -g '*.fig' + -g '*(-/)' xfig
compctl -g '*(-/) .*(-/)' cd
compctl -g '(^(*.o|*.class|*.jar|*.gz|*.gif|*.a|*.Z|*.bz2))' + -g '.*' less vim
compctl -g '(^(*.o|*.class|*.jar|*.gif|*.a))' + -g '.*' most
compctl -g '*.ltx' + -g '*(-/)' pdflatex
compctl -g '*.wav' auCDtect
compctl -g '*.fb2 *.fb2.zip' FBReader

autoload -U compinit
if [ $UID -eq 0 ]; then
    compinit -i -d ~/.zrootcompdump
else
    compinit -i
fi

# xterm header
case $TERM in
xterm*|rxvt*)
    precmd () {
        print -Pn "\033]0;%n@%M - %/\a"
        print -Pn "\033]1;%n@%m \a"
    }
    preexec () {
        print -Pn "\033]0;%n@%M - %/ - ($1)\a"
        print -Pn "\033]1;%n@%m \a"
    }
;;
screen)
    preexec () {
    # set screen title
        echo -ne "\ek${1[(w)1]}\e\\"
    }
    precmd () {
    #set screen title
        echo -ne "\ekzsh\e\\"
    }
;;
esac


# Search file, containing string in name
function ff() { ls -lhd **/*$** ; }

# rename file to lowercase
function lowercase()
{
    zmv "($1)" '${(L)1}'
}

function ram()
{
    if [ -z "$1" ]; then
        echo "First argument - pattern to grep from processes"
    else
        SUM=0
        for i in `ps aux|grep -i $1|awk '{print $6}'`; do
            SUM=`expr $i + $SUM`
        done
        echo $SUM
    fi
}

function split2flac {
    if [ -z "$2" ]; then
        echo "Usage: split2flac cue-file sound-file"
    else
        cuebreakpoints $1 | shnsplit -o flac $2
        cuetag $1 split-track*.flac
    fi
}

function myeditor {
    if [ -z `ps -C emacs -o pid=` ]; then
        vim ${@}
    else
        emacsclient -t -c ${@}
    fi
}

function gkill {
    awk '{print $2}'|xargs kill ${@}
}

#############        ALIASES         ###############
# Nocorrect
#alias mv="nocorrect mv"
#alias cp="nocorrect cp"
alias mkdir="nocorrect mkdir"

## LFTP
if [ -x "`whence -c lftp`" ]; then
    alias ftp="lftp"
    function sftp() { lftp sftp://`whoami`@$1 }
fi

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
    alias et="emacsclient -t"
    export ALTERNATE_EDITOR="vim"
else
    alias e=$EDITOR
fi

if [ -x "`whence -c rlwrap`" ]; then
    alias nc='rlwrap nc'
fi

# ls
if [ $(uname) = "Linux" ]; then
    alias ls="/bin/ls --color"
elif [ -x "$(whence -c gls)" ]; then
    alias ls="gls --color"
else
    alias ls="/bin/ls -G"
fi
alias ll="ls -lh"
alias la="ls -lA"
alias lsd="ls -ld *(-/DN)"
alias lsa="ls -ld .*"

# ZSH Directory Bookmarks
alias m1='alias g1="cd `pwd`"'
alias m2='alias g2="cd `pwd`"'
alias m3='alias g3="cd `pwd`"'
alias m4='alias g4="cd `pwd`"'
alias m5='alias g5="cd `pwd`"'
alias m6='alias g6="cd `pwd`"'
alias m7='alias g7="cd `pwd`"'
alias m8='alias g8="cd `pwd`"'
alias m9='alias g9="cd `pwd`"'
alias mdump='alias | awk "/^g[0-9]/ { print \"alias \" \$0 }" > ~/.bookmarks'
alias lm='alias | grep "^g[0-9]"'
touch ~/.bookmarks
source ~/.bookmarks

# Other
alias m="mutt"
alias rm="rm -f"
alias mc="mc -acx"
alias sd="screen -D -r"
alias l=$PAGER
alias g="egrep -i --color"
alias h="head"
alias p="ping"
alias df="df -h"
alias bc="bc -l"
alias myapg="apg -a 1 -n 8 -x 9 -M NCL -E l1iI0Oo"
alias rezsh="source ~/.zshrc"
alias apt="noglob sudo apt-get"
alias wa="wajig"
alias s="mdfind -name"
alias ri="ri -f ansi"
alias -g E='2>&1'
alias clive="noglob clive"
alias preview='groff -Tps | open -f -a Preview'
alias depyc='noglob find . -name *.pyc -delete'
alias ve='virtualenv --distribute --no-site-packages'
alias wget='wget --no-check-certificate'
alias ho="sudo vim /etc/hosts"
alias pc="rsync -P"

function mq() { hg --cwd $(hg root)/.hg/patches/ $@ }
function qser() { vim $(hg root)/.hg/patches/series }
function hgrc() { vim $(hg root)/.hg/hgrc }
function blog() { hg slog -l 500 -r "reverse(..'$1' - ..master)" $2 $3 $4 }
function bdiff() { hg diff -r "ancestor('$1', master)" -r "$1" $2 $3 $4 }

alias psc="ps -C"
alias psfg="ps -ylfC"
function psk() { ps -C $1 -o pid= | xargs kill }

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
        DPORT=${2-$1}
        echo "sapientisat.org:$DPORT"
        ssh -q -f -N -R 0.0.0.0:$DPORT:localhost:$1 sapientisat.org > /dev/null 2&>1
    fi
}

# for emacs' tramp
[[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ ' && unalias ls || return 0

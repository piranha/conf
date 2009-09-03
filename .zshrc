# -*- mode: sh -*-
# zsh configuration
# (c) 2003-2009 Alexander Solovyov
# piranha AT piranha.org.ua
#
# Thanks to:
# tldp.org
# Alexander Zayats
# "XAKEP" journal

export LANG=en_US.UTF-8
export LC_TIME=C
export LC_NUMERIC=C

stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

umask 022

export PATH=~/bin:/opt/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/bin:/usr/local/sbin
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

export PAGER="less"
if [ -x "`whence -c vim`" ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi
export BROWSER="links"
export LESS="FRQX"
export PS_FORMAT="user,group,pid,rss,sz,stime,time,cmd"

# local settings can override some settings
if [ -f ~/.zshlocal ]; then
    source ~/.zshlocal
fi

# Prompt setup (c) smax 2002, adapted for zsh (c) piranha 2004
# 0-black, 1-red, 2-green, 3-yellow, 4-blue, 5-magenta 6-cyan, 7-white
Cr() { echo '%{\033[3'$1'm%}'; }
hc=`Cr 6`; wc=`Cr 3`; tc=`Cr 7`; w=`Cr 7`; n=`Cr 9`; r=`Cr 1`; y=`Cr 6`; gr=`Cr 2`
[ $UID = 0 ] && at=$r%B'#'%b || at=$w'@'
err="%(?..$r%B%?%b )"
#PS1="$wc%n$at$hc%m $err$wc%~$w>$n"
# for white background
wc=`Cr 4`
PS1="$wc%n$n@$wc%m $err$wc%~>$n "
#export RPROMPT=$(echo "$gr%T$n")
unset n b Cr uc hc wc tc tty at r y gr

# make ls looks nice
#if [ -f ~/.dircolors ]; then
#    eval $(dircolors ~/.dircolors)
#fi
#export LSCOLORS="Cxfxcxdxbxegedabagacad"

fpath=(~/.zsh.d $fpath)

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# History
HISTFILE=~/.zhistory
HISTSIZE=5000
SAVEHIST=10000
setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt autocd
setopt auto_pushd
setopt pushd_ignore_dups

# Another important options
unsetopt extended_glob # it's quite annoying
setopt notify # report the status of backgrounds jobs immediately
setopt completeinword
setopt hash_list_all
REPORTTIME=5
watch=(notme root)

# ZLE
bindkey -e
bindkey "[2~" transpose-words
bindkey "[3~" delete-char
bindkey "[1~" beginning-of-line
bindkey "[4~" end-of-line
bindkey "[A" up-line-or-history
bindkey "[B" down-line-or-history
bindkey '^[z' delete-to-char
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line


# Loading builtins
zmodload zsh/deltochar
autoload -U zmv
# load on reference
zmodload -a zsh/zpty zpty

# press esc-e for editing command line in $EDITOR or $VISUAL
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line


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
compinit -i

# xterm header
case $TERM in
xterm*|rxvt*)
    precmd () {
        print -Pn "\033]0;%n@%M (%y) - %/\a"
        print -Pn "\033]1;%n@%m (tty%l)\a"
    }
    preexec () {
        print -Pn "\033]0;%n@%M (%y) - %/ - ($1)\a"
        print -Pn "\033]1;%n@%m (tty%l)\a"
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
#function ff() { find . -type f -iname '*'$*'*' -ls ; }
function ff() { ls -lh **/*$** ; }

# rename file to lowercase
function lowercase()
{
    zmv "($1)" '${(L)1}'
}

function isomake()
{
	if [ -z "$1" ]; then
		echo "isomake: first parameter - iso-file name"
		echo "isomake: second parameter - input dir/file name"
	else
		mkisofs -v -J -r -o $1 $2
	fi
}

function apt-show()
{
    if [ -z "$1" ]; then
        echo "First argument - name of package"
    else
        apt-cache show $1|egrep --color -v \(^Size\|^MD5sum\|^Filename\|^Suggests\|^SHA\|^Architecture\|^Maintainer\|^Section\|^Priority\)
    fi
}

# tail -f, possibly colorized
function t()
{
    if [ -x "`whence -c ccze`" ]; then
        tail -f $1 | ccze -A
    else
        tail -f $1
    fi
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
    awk '{print $2}'|xargs kill
}

# sorted du -sh
function duf {
    du -sk ${@} | sort -n | while read size fname; do
        for unit in k M G T P E Z Y; do
            if [ $size -lt 1024 ]; then
                echo -e "${size}${unit}\t${fname}"
                break
            fi
            size=$((size/1024))
        done
    done
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
## Mutt new generation
if [ -x "`whence -c muttng`" ]; then
        alias m="muttng"
else
        alias m="mutt"
fi
## color ls
if [ `uname` = "Linux" ]; then
        alias ls="/bin/ls --color"
else
        alias ls="/bin/ls -G"
fi
## GNU Find
if [ `uname` != "Linux" -a -x "`whence -c gfind`" ]; then
    alias find="noglob gfind"
else
    alias find="noglob find"
fi
## Editor
if [ -x "`whence -c emacsclient`" ]; then
    alias e="emacsclient --no-wait"
    alias et="emacsclient -t"
    export ALTERNATE_EDITOR="emacs"
else
    alias e=$EDITOR
fi

if [ -x "`whence -c rlwrap`" ]; then
    alias nc='rlwrap nc'
fi

# extensions
alias -s jpg=gliv
alias -s png=gliv
alias -s gif=gliv
alias -s flv=smplayer
alias -s avi=smplayer

# ls
alias ll="ls -lh"
alias la="ls -lA"
alias lsd="ls -ld *(-/DN)"
alias lsa="ls -ld .*"

# Other
alias rm="rm -f"
alias grep="egrep"
alias mc="mc -acx"
alias sd="screen -D -r"
alias l=$PAGER
alias g="egrep -i --color"
function gr() { egrep -i --color -r $1 . }
alias h="head"
alias p="ping"
alias df="df -h"
alias bc="bc -l"
alias slrn="slrn --nntp -h localhost"
alias ml="ledit -h ~/.mldonkey_history -x nc localhost 4000"
alias myapg="apg -n 8 -x 9 -M NCL -s"
alias yapg="apg -a 1 -n 8 -x 9 -M NCL -E l1iI0Oo"
alias -g C="|ccze -A"
alias rezsh="source ~/.zshrc"
alias cal="cal -m"
alias apt="noglob sudo apt-get"
alias wa="wajig"
alias s="sudo"
function mq() { hg --cwd $(hg root)/.hg/patches/ $@ }
function qser() { vim $(hg root)/.hg/patches/series }
function tran() { sdcv -u 'Universal (Ru-En)' -u 'LingvoUniversal (En-Ru)' $1 | sed "s/&apos;\|'//g" }

alias psc="ps -C"
alias psfg="ps -ylfC"
function psk() { ps -C $1 -o pid= | xargs kill }

# for emacs' tramp
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && unalias ls || return 0

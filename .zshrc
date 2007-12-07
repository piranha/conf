# -*- mode: sh; mode: fold -*-
# zsh configuration
# (c) 2003-2007 Alexander Solovyov
# piranha AT piranha.org.ua
#
# Thanks to:
# tldp.org
# Alexander Zayats
# "XAKEP" journal

# Loading builtins
zmodload zsh/deltochar
autoload -U zmv

#export LANG=ru_RU.koi8r
#export LANG=ru_RU.KOI8-R
#export TZ='Europe/Kiev'
if [ -f ~/.zshlocal ]; then
    source ~/.zshlocal
fi
export LC_TIME=C
export LC_NUMERIC=C

stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

umask 022

export PATH=~/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/bin:/usr/local/sbin:/usr/X11R6/bin
if [ -x `whence -c most` ]; then
        export PAGER="most"
else
        export PAGER="less"
fi
if [ -x `whence -c vim` ]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi
export BROWSER="links"
#export MAIL=~/.mail/
#export MAIL=/var/mail/piranha
#export MAIL=/home/hosting/eth0.net.ua/`whoami`/Maildir/
export LESS="-R"
export PERL5LIB=${PERL5LIB:+$PERL5LIB:}$HOME/perl
if [ -d $HOME/share/man ]
then
    export MANPATH=$HOME/share/man:$(manpath)
fi

# Prompt setup (c) smax 2002, adapted for zsh (c) piranha 2004
# 0-black, 1-red, 2-green, 3-yellow, 4-blue, 5-magenta 6-cyan, 7-white
Cr() { echo '%{\033[3'$1'm%}'; }
hc=`Cr 6`; wc=`Cr 3`; tc=`Cr 7`; w=`Cr 7`; n=`Cr 9`; r=`Cr 1`; y=`Cr 6`; gr=`Cr 2`
[ $UID = 0 ] && at=$r%B'#'%b || at=$w'@'
PS1="$wc%n$at$hc%m $wc%~$w>$n"
#export RPROMPT=$(echo "$gr%T$n")
unset n b Cr uc hc wc tc tty at r y gr

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# History
HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=1000
setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt autocd

# ZLE
bindkey -e
bindkey "[2~" transpose-words
bindkey "[3~" delete-char
bindkey "[1~" beginning-of-line
bindkey "[4~" end-of-line
bindkey "[A" up-line-or-history
bindkey "[B" down-line-or-history
bindkey '^[z' delete-to-char

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

######## Completition #######
#hostsmy=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
hosts=(${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*})
#???#zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*' 'm:{a-zA-Z}={A-Za-z} r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle :compinstall filename '.zshrc'

compctl -o wget make man rpm iptables
compctl -k hosts ssh telnet ping mtr traceroute
compctl -j -P "%" kill
compctl -g '*.gz' + -g '*(-/)' gunzip gzcat
compctl -g '*.rar' + -g '*(-/)' rar unrar
compctl -g '*(-*)' + -g '*(-/)' strip
compctl -g '*.ps *.eps' + -g '*(-/)' gs ghostview psnup psduplex ps2ascii
compctl -g '*.dvi' + -g '*(-/)' xdvi dvips
compctl -g '*.xpm *.xpm.gz' + -g '*(-/)' xpmroot sxpm pixmap xpmtoppm
compctl -g '*.fig' + -g '*(-/)' xfig
compctl -g '*(-/) .*(-/)' cd
compctl -g '(^(*.o|*.class|*.jar|*.gz|*.gif|*.a|*.Z|*.bz2))' + -g '.*' less vim
compctl -g '*.pkg.tar.gz' pacman
#compctl -g '*' + -g '.*' vim
#compctl -g '*.html' + -g '*(-/)' appletviewer

autoload -U compinit
compinit
# End of lines added by compinstall

# Search file, containing string in name
function ff() { find . -type f -iname '*'$*'*' -ls ; }

# Search file, containing string inside
function fstr()
{
    OPTIND=1
    local case=""
    local usage="fstr: search string in files.
Usage: fstr [-i] \"string_template\" [\"filename_template\"] "
    while getopts :it opt
    do
        case "$opt" in
        i) case="-i " ;;
        *) echo "$usage"; return;;
        esac
    done
    shift $(( $OPTIND - 1 ))
    if [ "$#" -lt 1 ]; then
        echo "$usage"
        return;
    fi
    local SMSO=$(tput smso)
    local RMSO=$(tput rmso)
    find . -type f -name "${2:-*}" -print0 | xargs -0 grep -sn ${case} "$1" 2>&- | egrep --color $1
}

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
    if [ -x `whence -c ccze` ]; then
        tail -f $1 | ccze -A
    else
        tail -f $1
    fi
}

function totalram()
{
    if [ -z "$1" ]; then
        echo "First argument - pattern to grep from processes"
    else
        SUM=0
        for i in `ps aux|grep $1|awk '{print $5}'`; do
            SUM=`expr $i + $SUM`
        done
        echo $SUM
    fi
}

#############        ALIASES         ###############
# Nocorrect
#alias mv="nocorrect mv"
#alias cp="nocorrect cp"
alias mkdir="nocorrect mkdir"

# Recode aliases
alias w2k="iconv -c -f cp1251 -t koi8-r"
alias k2w="iconv -c -f koi8-r -t cp1251"
alias u2k="iconv -c -f utf-8 -t koi8-r"
alias k2u="iconv -c -f koi8-r -t utf-8"
alias U2k="iconv -c -f utf-16 -t koi8-r"
alias k2U="iconv -c -f koi8-r -t utf-16"

## LFTP
if [ -x `whence -c lftp` ]; then
    alias ftp="lftp"
    function sftp() { lftp sftp://`whoami`@$1 }
fi
## Mutt new generation
if [ -x `whence -c muttng` ]; then
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
if [ `uname` != "Linux" -a -x `whence -c gfind` ]; then
    alias find="noglob gfind"
else
    alias find="noglob find"
fi
## Editor
if [ -x `whence -c emacsclient` ]; then
    alias e="emacsclient --no-wait"
    alias et="emacsclient -t"
    export ALTERNATE_EDITOR="emacs"
else
    alias e=$EDITOR
fi

if [ -x `whence -c rlwrap` ]; then
    alias nc='rlwrap nc'
fi

# extensions
alias -s jpg=gliv
alias -s flv=mplayer
alias -s avi=mplayer

# ls
alias ll="ls -lh"
alias la="ls -lA"
alias lsd="ls -ld *(-/DN)"
alias lsa="ls -ld .*"

# Other
alias rm="rm -f"
alias grep="egrep"
alias mc="mc -acx"
alias ss="sudo -s"
alias sr="screen -D -r"
alias l=$PAGER
alias g="egrep -i --color"
alias c="cat"
alias h="head"
alias p="ping"
alias df="df -h"
alias bc="bc -l"
alias slrn="slrn --nntp -h localhost"
alias ml="ledit -h ~/.mldonkey_history -x nc localhost 4000"
alias myapg="apg -n 8 -x 9 -M NCL -s"
alias yapg="apg -a 1 -n 8 -x 9 -M NCL -E l1iI0Oo"
alias mkperlpkg="dh-make-perl --build --cpan"
alias -g C="|ccze -A"
alias rezsh="source ~/.zshrc"
alias ql="quodlibet"
alias cal="cal -m"
alias psg="ps -ylC"
alias psfg="ps -ylfC"

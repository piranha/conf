# -*- mode: sh -*-
# zsh configuration
# (c) 2003-2013 Alexander Solovyov
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

export PATH=~/bin:/usr/local/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
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
export LEIN_FAST_TRAMPOLINE=y
export FZF_DEFAULT_OPTS="--ansi"

# local settings can override some settings
if [ -f ~/.zshlocal ]; then
    source ~/.zshlocal
fi

# Prompt setup (c) smax 2002, adapted for zsh (c) piranha 2004, 2013
# 0-black, 1-red, 2-green, 3-yellow, 4-blue, 5-magenta 6-cyan, 7-white
Cr() { echo '%{\033[3'$1'm%}'; }
hc=`Cr 6`; wc=`Cr 3`; tc=`Cr 7`; w=`Cr 7`; n=`Cr 9`; r=`Cr 1`; y=`Cr 6`; gr=`Cr 2`
[ $UID = 0 ] && at=$r%B'#'%b || at='@'
#PS1="$wc%n$at$hc%m $err$wc%~$w>$n"
# for white background
wc=`Cr 4`
# nbsp allows to have space which will delete everything before itself when
# pasted - so it's possible to copy and paste whole line with prompt and have
# only command left
nbsp=$'\u00A0'
bindkey -s $nbsp '^u'
PS1="$wc%n$n$at$wc%m $err$wc%~>$n$nbsp"
unset n b Cr uc hc wc tc tty at r y gr nbsp

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
setopt printexitvalue
REPORTTIME=5
watch=(notme root)

# Loading builtins
autoload -U zmv

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
        print -Pn "\e]0;@%M - %/\a"
        print -Pn "\e]1;@%m \a"
    }
    title_preexec () {
        print -Pn "\e]0;@%M - %/: $1\a"
        print -Pn "\e]1;@%m: $1 \a"
    }
;;
screen)
    title_precmd () {
        echo -ne "\ekzsh\e\\"
    }
    title_preexec () {
        echo -ne "\ek${1[(w)1]}\e\\"
    }
;;
esac

precmd_functions=(title_precmd)
preexec_functions=(title_preexec)

if [[ -o login ]]; then if [ x"$TERM" != "xscreen" ]; then
    # Indicates start of command output. Runs just before command executes.
    iterm2_before_cmd_executes() {
        printf "\033]133;C;\r\007"
    }

    iterm2_set_user_var() {
        printf "\033]1337;SetUserVar=%s=%s\007" "$1" $(printf "%s" "$2" | base64)
    }

    # Users can write their own version of this method. It should call
    # iterm2_set_user_var but not produce any other output.
    # e.g., iterm2_set_user_var currentDirectory $PWD
    # Accessible in iTerm2 (in a badge now, elsewhere in the future) as
    # \(user.currentDirectory).
    iterm2_print_user_vars() {
        iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
    }

    iterm2_print_state_data() {
        printf "\033]1337;RemoteHost=%s@%s\007" "$USER" "$iterm2_hostname"
        printf "\033]1337;CurrentDir=%s\007" "$PWD"
        iterm2_print_user_vars
    }

    # Report return code of command; runs after command finishes but before prompt
    iterm2_after_cmd_executes() {
        printf "\033]133;D;$?\007"
        iterm2_print_state_data
    }

    # Mark start of prompt
    iterm2_prompt_start() {
        printf "\033]133;A\007"
    }

    # Mark end of prompt
    iterm2_prompt_end() {
        printf "\033]133;B\007"
    }

    iterm2_precmd() {
        iterm2_after_cmd_executes

        # The user or another precmd may have changed PS1 (e.g., powerline-shell).
        # Ensure that our escape sequences are added back in.
        if [[ "$ITERM2_SAVED_PS1" != "$PS1" ]]; then
            PS1="%{$(iterm2_prompt_start)%}$PS1%{$(iterm2_prompt_end)%}"
            ITERM2_SAVED_PS1="$PS1"
        fi
    }

    iterm2_preexec() {
        PS1="$ITERM2_SAVED_PS1"
        iterm2_before_cmd_executes
    }

    # If hostname -f is slow on your system, set iterm2_hostname prior to sourcing this script.
    [[ -z "$iterm2_hostname" ]] && iterm2_hostname=`hostname -f`

    precmd() {
        iterm2_precmd
    }

    preexec() {
        iterm2_preexec
    }

    iterm2_print_state_data
    printf "\033]1337;ShellIntegrationVersion=1\007"
fi; fi

d() {
    local dir
    select dir in $dirstack; do
        echo $dir
        break
    done
    test "x$dir" != x && cd $dir
}

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
    alias ec="emacsclient -t"
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
alias lt="ls -lt"
alias la="ls -lA"
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
alias mc="mc -acx"
alias sd="screen -D -r"
#alias l=$PAGER
alias g="egrep -i --color"
alias h="head"
alias t="tail -f"
alias p="ping"
alias df="df -h"
alias bc="bc -l"
alias myapg="apg -a 1 -n 8 -x 9 -M NCL -E l1iI0Oo"
alias rezsh="source ~/.zshrc"
alias s="mdfind -name"
alias ri="ri -f ansi"
alias -g N='2>&1'
alias -g X='| xargs '

alias ho="sudo vim /etc/hosts"
alias pc="rsync -P"
alias sudo="sudo " # this carries aliases to sudo calls
alias youtube-dl='noglob youtube-dl'
alias wget='noglob wget'
alias curl='noglob curl'

function bdiff() { hg diff -r "ancestor('$1', master)" -r "$1" $2 $3 $4 }

alias pg="pgrep -lf"
function pgk() { pgrep -f $1 | xargs kill }
alias -g B='$(git symbolic-ref HEAD)'
alias master="git checkout master"
alias u="underscore"
alias gf="gr -f"

function fe() {
    if [ -z "$1" ]; then
        gr -cf . | fzf --select-1 | xargs emacsclient --no-wait
    else
        gr -f "$1" | fzf --select-1 | xargs emacsclient --no-wait
    fi
}

function ge() {
    gr -cn "$1" 2>/dev/null | fzf --select-1 | xargs emacsclient --no-wait
}

function fd() {
  local dir
  dir=$(find . -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf +m --select-1 -q "${1}") && cd "$dir"
}

function cfd() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

function gitshow() {
  local out sha q
  while out=$(
      git log --decorate=short --graph --oneline --color=always |
      fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
    q=$(head -1 <<< "$out")
    while read sha; do
      [ -n "$sha" ] && git show --color=always $sha | less -R
    done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
  done
}

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
        echo "maia.solovyov.net:$DPORT"
        ssh -N -R "*:${DPORT}:localhost:${1}" maia.solovyov.net
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

alias dummy_email='python -m smtpd -n -c DebuggingServer localhost:1026'

# for emacs' tramp
[[ $TERM = "dumb" ]] && unsetopt zle && PS1='$ ' && unalias ls || return 0

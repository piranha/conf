# -*- mode: sh -*-
# zsh configuration
# (c) 2003-2025 Oleksandr Solovyov
# o AT solovyov.net

export LANG=en_US.UTF-8
export LC_TIME=C
export LC_NUMERIC=C

[[ $TERM = "xterm" ]] && stty pass8

unlimit
limit stack 8192
limit core 0
limit -s

if [[ $(uname -s) = Darwin ]]; then
    # when it's unlimited, it's not. Go figure. :\
    ulimit -S -n 4096
fi

umask 022

export PATH=~/bin:~/.local/bin:/usr/local/go/bin:/opt/homebrew/sbin:/opt/homebrew/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
#export DYLD_LIBRARY_PATH="/opt/homebrew/lib" #:$DYLD_LIBRARY_PATH"
#export LD_LIBRARY_PATH="/opt/homebrew/lib:$LD_LIBRARY_PATH"
#export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig:$PKG_CONFIG_PATH"
#test -x "/bin/launchctl" && sudo launchctl setenv PATH $PATH

# for emacs' tramp
if [[ $TERM == "dumb" ]]; then
  unsetopt zle
  PS1='$ '
  return
fi

export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=34"
export LSCOLORS="exfxcxdxbxegedabagacex"

### Prompt

p_color='%F{blue}'
p_at='%(!.%F{red}%B#%b%f.@)'
p_host="${p_color}%m%f"
p_path="${p_color}%~%f"
p_invite="%(?.${p_color}>%f.%F{red}×%f)"

PS1="$p_at$p_host $p_path$p_invite "
unset p_color p_at p_host p_path p_invite

setopt promptsubst

### History

fpath=(~/.zsh.d /opt/homebrew/share/zsh/site-functions /opt/homebrew/share/zsh-completions $fpath)

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# History
HISTFILE=~/.zhistory
HISTSIZE=10000
SAVEHIST=10000
setopt inc_append_history
setopt no_share_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt autocd
setopt auto_pushd pushd_ignore_dups pushd_silent pushd_to_home

# other important options
#unsetopt extended_glob # it's quite annoying
setopt notify # report the status of backgrounds jobs immediately
setopt completeinword
setopt hash_list_all
#setopt printexitvalue
REPORTTIME=10
watch=(notme root)

# Loading builtins
autoload -U zmv

### ZLE
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

# press meta-e for editing command line in $EDITOR or $VISUAL
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

autoload -U select-word-style
select-word-style normal
# don't contains -_/= - and thus breaks words on them
export WORDCHARS='*?[]~&;!#$%^(){}<>'

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

compctl -o wget make man
compctl -j -P "%" kill
compctl -g '*(-*)' + -g '*(-/)' strip
compctl -g '*(-/) .*(-/)' cd

### Custom completion setup

function _say_voices {
    reply=(${(f)"$(say -v '?' | awk -F '  ' '{print $1}')"})
}
# `-x 'c[-1,-v]'` means "if prev (-1) word is '-v', then ..."
# `-M ...` means "ignore case"
compctl -x 'c[-1,-v]' -M 'm:{a-zA-Z}={A-Za-z}' -K _say_voices -- say

##### end of completion #####

## xterm header
## \e]0; will set window's and tab's header
## \e]1; will set tab's header
title_precmd () {
    print -Pn "\e]0;@%m:%~\a"
    print -Pn "\e]1;@%m:%~\a"
}
title_preexec () {
    print -Pn "\e]1;@%m:$1 - %~\a"
}
precmd_functions+=(title_precmd)
preexec_functions+=(title_preexec)

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
alias make="env -u CDPATH make" # make *hates* CDPATH

## GNU Find
if [[ `uname` != "Linux" && -x `whence gfind` ]]; then
    alias find="gfind"
fi

## Editor
if [[ -x $(whence emacsclient) ]]; then
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

if [[ -x $(whence rlwrap) ]]; then
    alias nc='rlwrap nc'
fi

# ls
alias ls="ls --color"
alias ll="ls -lh"
alias lt="ls -lt"
alias la="ls -lA"

function l() {
    if [ $# -gt 0 -a -d $1 ]; then
        ls $@
    else
        $PAGER $@
    fi
}

# Other
alias rm="rm -f"
alias g="egrep -i --color"
alias h="head"
alias t="tail -f"
alias mypw="pwgen -1Bny 10 10"
alias rezsh="exec zsh"
alias s="mdfind -name"

alias ho="sudo vim /etc/hosts"

alias -g B='$(git symbolic-ref HEAD)'
alias master="git checkout master"

# find file by name and open it in emacs
function fe() {
    fd -t f "$1" | fzf --print0 | xargs -0 emacsclient --no-wait
}

# find file by content, then filter it by name and open it in emacs
function ge() {
    rg -n "$*" | fzf --tac | IFS=: read e_file e_line e_rest
    emacsclient -n +${e_line:-1} "$e_file"
}
compdef ge=rg

# find file by name and switch to directory containing that file
function cfd() {
    local file=$(fzf -q "$1")
    cd $(dirname "$file")
}

### repo dir jumper

function gro() { # go root
    cd $(git rev-parse --show-toplevel)
}

function gd() { # go directory
    local dir=$(fd --type f --hidden --exclude .git | fzf --preview "ls -Ap {}")
    [[ -n "$dir" ]] && cd "$dir"
}

alias gg="gro && gd"

### /repo dir jumper

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

### netrc
function _list_netrc() {
    reply=($(cut -d ' ' -f 2 < ~/.netrc))
}

function netrc() {
    cat ~/.netrc | perl -ne "/^machine $argv[1]( login ([^ ]+))?( password ([^ ]+))?/ && print defined(\$2) ? \$2.':'.\$4 : \$4" | head -1
}
compctl -K _list_netrc netrc

### /netrc

### empty enter expander

empty-enter-expander () {
    if [[ ! ( -z $BUFFER && $CONTEXT == "start" ) ]]; then
        zle accept-line
        return
    fi

    clear
    local target=~/.zsh.d/enter
    local key
    local exit_loop=false

    handle_key() {
        local key="$1"
        local dst=$(find "$target" -mindepth 1 -maxdepth 1 -name "$key *")
        if [[ -d "$dst" ]]; then
            target="$dst"
            clear
            return
        fi
        if [[ -f "$dst" ]]; then
            clear
            cat "$dst"
            source "$dst"
            stop
            return
        fi
        clear
        echo 'ERROR: Invalid selection'
    }

    stop() {
        # clear
        zle reset-prompt
        exit_loop=true
    }

    while true; do
        echo ' ===> [ Empty Enter Expander ]'

        if [[ -f "$target/.exec" ]]; then
            zsh "$target/.exec"
        fi

        /bin/ls -1 -- "$target"
        echo -n "\n ? "

        read -k key
        #printf 'You pressed: "%s" (hex: %02X)\n' "$key" "'$key"
        case "$key" in
            $'\C-d') stop ;;
            [a-zA-Z0-9]) handle_key "$key" ;;
            *) clear; printf 'ERROR: Unknown key "%s" (hex: %02X)\n' "$key" "'$key" ;;
        esac

        $exit_loop && return || continue
    done
}
zle -N empty-enter-expander
#bindkey "^M" empty-enter-expander
### /empty enter expander

function makegif() {
    convert -delay 1x25 *.png -ordered-dither o8x8,9 -coalesce -layers OptimizeTransparency +map -crop 480x270+0+45 +repage animation.gif
}

function Q() {
    psql service="$1" "${@:2}"
}

umedit() { [ -z "$1" ] && return 1; mkdir -p ~/Documents/kb; vim ~/Documents/kb/"$1.md" }
um() { pandoc -s -t man ~/Documents/kb/"$1.md" | tbl | groff -Wall -mtty-char -man -Tascii -c | less -R }

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

ttfb() { curl -s -o /dev/null -w "Connect: %{time_connect} TTFB: %{time_starttransfer} Total time: %{time_total} Size: %{size_download}\n" "$@" }
alias -g UA="-H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0'"
checkdomain() { whois "$1" | grep -iE 'no match|creation date' }

alias ccc='claude --dangerously-skip-permissions'

## Interactive setup

if [[ -o interactive ]]; then
    setopt auto_cd
    cdpath=(~/dev/work ~/dev/web ~/dev/misc ~/dev/fpv)
fi

export PAGER="less"
if [[ -x "$(whence vim)" ]]; then
    export EDITOR="vim"
else
    export EDITOR="vi"
fi
export LESS="FRQXi"
export PS_FORMAT="user,group,pid,rss,sz,stime,time,cmd"
export FZF_DEFAULT_OPTS="--ansi --color light --preview 'head -100 {}' --select-1"
export FZF_DEFAULT_COMMAND="fd -t f"
export FZF_CTRL_T_OPTS="$FZF_DEFAULT_OPTS"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_ENV_HINTS=1

export OPENAI_API_KEY="$(netrc api.openai.com)"
export ANTHROPIC_API_KEY="$(netrc api.anthropic.com)"
export GEMINI_API_KEY="$(netrc generativelanguage.googleapis.com)"
export MISTRAL_API_KEY="$(netrc api.mistral.ai)"
export XAI_API_KEY="$(netrc api.x.ai)"

### Various local crap

# local settings can override some settings
if [ -f ~/.zshlocal ]; then source ~/.zshlocal; fi
if [ -f ~/dev/misc/try/try ]; then eval "$(~/dev/misc/try/try init)"; fi
if [ -f ~/.fzf.zsh ]; then source ~/.fzf.zsh; fi
if [ -x "$(whence zoxide)" ]; then
    eval "$(zoxide init zsh)"
    alias zq='zoxide query'
fi

# bun completions
[ -s "/Users/sansolo/.bun/_bun" ] && source "/Users/sansolo/.bun/_bun"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# Ghostty shell integration: automatic via ZDOTDIR trick on first launch.
# For `exec zsh` / rezsh, manually source if _ghostty_state is not yet set.
if [[ -n "${GHOSTTY_RESOURCES_DIR}" ]] && (( ! ${+_ghostty_state} )); then
    builtin source "${GHOSTTY_RESOURCES_DIR}/shell-integration/zsh/ghostty-integration"
fi

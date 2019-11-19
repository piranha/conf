E:LANG = en_US.UTF-8
E:LC_TIME = C
E:LC_NUMERIC = C

E:GOPATH = ~/dev/go

paths = [$E:GOPATH/bin /Applications/Emacs.app/Contents/MacOS/bin ~/bin /usr/local/go/bin /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /bin]
if (has-external /bin/launchctl) {
  sudo launchctl setenv PATH $E:PATH
}

# when you set unlimited, macos limits open files to a low number
ulimit -S -n 4096

### Keybindings

use readline-binding
edit:insert:binding['Ctrl-]'] = { edit:location:start }


### Completion

use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/git
use github.com/zzamboni/elvish-completions/ssh


### Title

use github.com/zzamboni/elvish-modules/terminal-title

title-during-prompt = {
  put (tilde-abbr $pwd)
}


### Apps config

E:PAGER = less
E:LESS = FRQXi

E:FZF_DEFAULT_OPTS="--ansi --color light --preview 'head -100 {}' --select-1"
E:FZF_DEFAULT_COMMAND="fd -t f"
E:FZF_CTRL_T_OPTS="$FZF_DEFAULT_OPTS"
E:FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

E:HOMEBREW_CASK_OPTS="--appdir=/Applications"
E:EXA_COLORS="*.md=black:Makefile=black"


### Prompt

edit:prompt = { edit:styled (tilde-abbr $pwd) blue; edit:styled '> ' blue }
edit:rprompt = (constantly (whoami)@(hostname -s))


### Utilities

fn first-or-nil [@arr]{
  if (> (count $arr) 0) {
    put $arr[0]
  } else {
    put ""
  }
}


### Aliases

fn e [f]{ e:emacsclient --no-wait $f }

fn ho { sudo vim /etc/hosts }

fn ls [@a]{ e:exa $@a }
fn ll [@a]{ e:exa -l --git $@a }
fn lt [@a]{ e:exa -lm $@a }
fn la [@a]{ e:exa -la $@a }

fn fe [@args]{
  needle = (first-or-nil $args)
  if (eq $needle "") {
    fd -t f | fzf --print0 | xargs -0 emacsclient --no-wait
  } else {
    fd -t f $needle | fzf --print0 | xargs -0 emacsclient --no-wait
  }
}


fn Q [dbname @a]{
  psql service=$dbname $@a
}


### ES

fn POST   [@a]{ curl -n -H 'Content-Type: application/json' -XPOST   "$@" }
fn PUT    [@a]{ curl -n -H 'Content-Type: application/json' -XPUT    "$@" }
fn GET    [@a]{ curl -n -H 'Content-Type: application/json' -XGET    "$@" }
fn DELETE [@a]{ curl -n -H 'Content-Type: application/json' -XDELETE "$@" }


# cdpath not working: https://github.com/elves/elvish/issues/341

cdpaths = [~/dev/work ~/dev/web]
fn j [dir]{
  for p $cdpaths {
    if ?(test -d $p/$dir) {
      cd $p/$dir
      return
    }
  }
  cd $dir
}

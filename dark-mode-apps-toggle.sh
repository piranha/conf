#!/bin/sh

set -euo pipefail

kitten theme --reload-in=all "$(test "$DARKMODE" -eq 1 && echo 'Modus Vivendi' || echo 'Modus Operandi')"
emacsclient -n -e '(set-gui-theme!)'

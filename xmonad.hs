-- xmonad.hs
import Graphics.X11
import System.IO (hPutStrLn)
import qualified Data.Map as M
import Data.Bits ((.|.))
-- XMonad
import XMonad hiding ((|||))
import XMonad.ManageHook
import XMonad.Operations
import qualified XMonad.StackSet as W
-- Extensions
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

statusBarCmd = "xmobar"
-- statusBarCmd = "mymon.sh | dzen2 -bg '#3f3c6d' -fg '#a8a3f7' -sa c -e 'button1=togglecollapse' -fn '-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*' -ta l -xs 1 -u"

-- prompt config
ownXPConfig :: XPConfig
ownXPConfig = defaultXPConfig
              { font              = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
              , bgColor           = "#3f3c6d"
              , fgColor           = "#a8a3f7"
              , fgHLight          = "#a8a3f7"
              , bgHLight          = "blue"
              , borderColor       = "#FFFFFF"
              }

-- tabbed layout config
tabbedConf = defaultTheme { fontName = "-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*-*" }

-- dynamic log config
ownPP h = defaultPP
          { ppOutput = hPutStrLn h
          , ppCurrent = xmobarColor "white" "" . wrap "[" "]"
          , ppVisible = xmobarColor "yellow" "" . wrap "*" "*"
          , ppUrgent = xmobarColor "red" ""
          , ppTitle = xmobarColor "#00ee00" ""
          , ppSep    = " "
          , ppLayout = (\x -> "")
          }

playToggle = "mpc toggle"
playPrevious = "mpc prev"
playNext =  "mpc next"
playOrder = "mpc random"
playWindow = "quodlibet --toggle-window"

-- keys config
addKeys =
    [ ("M-f"   , sendMessage ToggleLayout)
    , ("M-C-x" , xmonadPrompt ownXPConfig)
    , ("M-<F5>", spawn playPrevious)
    , ("M-<F6>", spawn playNext)
    , ("M-<F7>", spawn playOrder)
    , ("M-<F8>", spawn playToggle)
    , ("M-a q" , sendMessage $ JumpToLayout "tiled")
    , ("M-a w" , sendMessage $ JumpToLayout "tabbed")
    , ("M-a a" , sendMessage $ JumpToLayout "accordion")
    , ("M-<Page_Up>", spawn "xclip -selection PRIMARY -o | xclip -selection CLIPBOARD -i")
    , ("M-<Page_Down>", spawn "xclip -selection CLIPBOARD -o | xclip -selection PRIMARY -i")
    , ("M-<F2>", windowPromptGoto ownXPConfig)
    , ("M-<F3>", sshPrompt ownXPConfig)
    , ("M-<Insert>", spawn "import -window root ~/screenshot-`date +\"%F--%H-%M-%S\"`.png")
    , ("M-<Pause>", spawn "gnome-screensaver-command --lock")
    , ("M-<Return>", dwmpromote)
    , ("M-<Right>", withFocused (keysMoveWindow (20, 0)))
    , ("M-<Left>", withFocused (keysMoveWindow (-20, 0)))
    , ("M-<Up>", withFocused (keysMoveWindow (0, -20)))
    , ("M-<Down>", withFocused (keysMoveWindow (0, 20)))
    , ("M-S-<Right>", withFocused (keysResizeWindow (20, 0) (0, 0)))
    , ("M-S-<Left>", withFocused (keysResizeWindow (-20, 0) (0, 0)))
    , ("M-S-<Up>", withFocused (keysResizeWindow (0, -20) (0, 0)))
    , ("M-S-<Down>", withFocused (keysResizeWindow (0, 20) (0, 0)))
    , ("M-<Backspace>", focusUrgent)
    , ("M-<F12>", spawn "amixer -q set PCM 2dB+")
    , ("M-<F11>", spawn "amixer -q set PCM 2dB-")
    ]


ownManageHook = composeAll . concat $
                [ [ className =? c --> doFloat | c <- floats]
                , [ className =? "Gecko" --> doF (W.shift "web") ]]
    where floats = ["MPlayer", "Gimp", "qiv", "Galculator", "Gcalctool"]


ownLayoutHook = smartBorders
                $ toggleLayouts (noBorders Full)
                $ named "tiled" tiled
                ||| named "mirror" (Mirror tiled)
                ||| named "tabbed" (noBorders owntab)
                ||| named "accordion" Accordion
    where
      owntab = tabbed shrinkText tabbedConf
      tiled = Tall 1 (3/100) (1/2)


-- overall config
ownConfig statusbar = defaultConfig
                { borderWidth        = 1
                , normalBorderColor  = "#3f3c6d"
                , focusedBorderColor = "#FF0000"
                , terminal           = "urxvtc"
                , defaultGaps        = [(18,0,0,0), (18,0,0,0)]
                , modMask            = mod4Mask
                , logHook            = dynamicLogWithPP $ ownPP statusbar
                , layoutHook         = ownLayoutHook
                , manageHook         = ownManageHook
                }

main = do statusbar <- spawnPipe statusBarCmd
          xmonad $ ownConfig statusbar `additionalKeysP` addKeys

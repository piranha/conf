-- xmonad.hs
-- (c) Alexander Solovyov 2007-2008
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
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad


statusBarCmd = "xmobar"
--statusBarCmd = "cat"

-- prompt config
ownXPConfig :: XPConfig
ownXPConfig = defaultXPConfig
              { font              = "xft:Consolas:size=12"
              , bgColor           = "#3f3c6d"
              , fgColor           = "#a8a3f7"
              , fgHLight          = "#a8a3f7"
              , bgHLight          = "blue"
              , borderColor       = "#FFFFFF"
              , position          = Top
              }

-- tabbed layout config
tabbedConf = defaultTheme { fontName = "xft:Consolas:size=12" }

-- dynamic log config
ownPP h = defaultPP
          { ppOutput = hPutStrLn h
          , ppCurrent = xmobarColor "white" "" . wrap "[" "]"
          , ppVisible = xmobarColor "yellow" "" . wrap "*" "*"
          , ppUrgent = xmobarColor "red" "" . wrap "|" "|"
          , ppTitle = xmobarColor "#00ee00" ""
          , ppSep    = " "
          , ppLayout = (\x -> "")
          }

-- keys config
addKeys =
    [ ("M-f"   , sendMessage ToggleLayout)
    , ("M-b"   , sendMessage ToggleStruts)
    , ("M-C-x" , xmonadPrompt ownXPConfig)
    , ("M-s"   , spawn "urxvtc -title scratchpad")
    , ("M-a q" , sendMessage $ JumpToLayout "tiled")
    , ("M-a w" , sendMessage $ JumpToLayout "tabbed")
    , ("M-<F2>", windowPromptGoto ownXPConfig)
    , ("M-<F3>", sshPrompt ownXPConfig)
    , ("M-<Right>", withFocused (keysMoveWindow (20, 0)))
    , ("M-<Left>", withFocused (keysMoveWindow (-20, 0)))
    , ("M-<Up>", withFocused (keysMoveWindow (0, -20)))
    , ("M-<Down>", withFocused (keysMoveWindow (0, 20)))
    , ("M-S-<Right>", withFocused (keysResizeWindow (20, 0) (0, 0)))
    , ("M-S-<Left>", withFocused (keysResizeWindow (-20, 0) (0, 0)))
    , ("M-S-<Up>", withFocused (keysResizeWindow (0, -20) (0, 0)))
    , ("M-S-<Down>", withFocused (keysResizeWindow (0, 20) (0, 0)))
    , ("M-<Backspace>", focusUrgent)
    , ("M-p", shellPrompt ownXPConfig)
      -- non-wm-specific binds
    , ("M-<Page_Up>", spawn "xclip -selection PRIMARY -o | xclip -selection CLIPBOARD -i")
    , ("M-<Page_Down>", spawn "xclip -selection CLIPBOARD -o | xclip -selection PRIMARY -i")
    , ("M-<Insert>", spawn "import -window root ~/screenshot-`date +\"%F--%H-%M-%S\"`.png")
    , ("M-<F11>", spawn "setxkbmap -option '' 'us'")
    , ("M-<F12>", spawn "setxkbmap -option 'grp:alt_shift_toggle' 'us,ua(winkeys)'")
    ]

mmKeys =
    [ ((0 , 0x1008ff11), spawn "amixer sset Master 2%-")    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff12), spawn "amixer sset Master toggle") -- XF86AudioMute
    , ((0 , 0x1008ff13), spawn "amixer sset Master 2%+")    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff14), spawn "amarok -t") -- XF86AudioPlay
    , ((0 , 0x1008ff15), spawn "amarok -s") -- XF86AudioStop
    , ((0 , 0x1008ff16), spawn "amarok -r") -- XF86AudioPrev
    , ((0 , 0x1008ff17), spawn "amarok -f") -- XF86AudioNext
    ]


ownManageHook = scratchpadManageHookDefault <+>
              (composeAll . concat $
                [ [ className =? c --> doFloat | c <- floats]
                , [ className =? "Gecko" --> doF (W.shift "web") ] ])
    where floats = ["MPlayer", "Gimp", "qiv", "Galculator", "Gcalctool"]


ownLayoutHook = smartBorders
                $ avoidStruts
                $ toggleLayouts (noBorders Full)
                $ named "tiled" tiled
                ||| named "mirror" (Mirror tiled)
                ||| named "tabbed" (noBorders owntab)
    where
      owntab = tabbed shrinkText tabbedConf
      tiled = Tall 1 (3/100) (1/2)


-- overall config
ownConfig statusbar = defaultConfig
                { borderWidth        = 1
                , normalBorderColor  = "#3f3c6d"
                , focusedBorderColor = "#FF0000"
                , terminal           = "urxvtc"
                , modMask            = mod4Mask
                , logHook            = dynamicLogWithPP $ ownPP statusbar
                , layoutHook         = ownLayoutHook
                , manageHook         = ownManageHook
                }

main = do statusbar <- spawnPipe statusBarCmd
          xmonad $ ownConfig statusbar `additionalKeysP` addKeys `additionalKeys` mmKeys

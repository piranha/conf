-- xmonad.hs
import Graphics.X11
import System.IO (hPutStrLn)
import qualified Data.Map as M
import Data.Bits ((.|.))
-- XMonad
import XMonad
import XMonad.ManageHook
import XMonad.Operations
import qualified XMonad.StackSet as W
-- Extensions
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WmiiActions
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Accordion
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

statusBarCmd = "xmobar"

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
tabbedConf :: TConf
tabbedConf = defaultTConf { fontName = "-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*-*" }

-- dynamic log config
ownPP h = defaultPP
          { ppOutput = hPutStrLn h
          , ppCurrent = xmobarColor "white" "" . wrap "[" "]"
          , ppVisible = xmobarColor "yellow" "" . wrap "*" "*"
          , ppTitle = xmobarColor "#00ee00" ""
          , ppSep    = " "
          , ppLayout = (\x -> "")
          }

playToggle = "quodlibet --play-pause"
playPrevious = "quodlibet --previous"
playNext =  "quodlibet --next"
playOrder = "quodlibet --order=toggle"
playWindow = "quodlibet --toggle-window"

-- keys config
ownKeys conf@(XConfig {modMask = modMask}) = M.fromList $
    [ ((modMask   ,              xK_f      ), sendMessage ToggleLayout)
    , ((modMask,                 xK_x      ), submap . M.fromList $
       [ ((0, xK_z), spawn playPrevious)
       , ((0, xK_x), spawn playToggle)
       , ((0, xK_c), spawn playOrder)
       , ((0, xK_v), spawn playNext)
       , ((0, xK_a), spawn playWindow)
       ])
    , ((modMask,                 xK_F5     ), spawn playPrevious)
    , ((modMask,                 xK_F6     ), spawn playNext)
    , ((modMask,                 xK_F7     ), spawn playOrder)
    , ((modMask,                 xK_F8     ), spawn playToggle)
    , ((modMask .|. controlMask, xK_x      ), xmonadPrompt ownXPConfig)
    , ((modMask,                 xK_F1     ), windowPromptGoto ownXPConfig)
    , ((modMask,                 xK_F2     ), shellPrompt ownXPConfig)
    , ((modMask,                 xK_F3     ), sshPrompt ownXPConfig)
    , ((modMask,                 xK_Insert ), spawn "import -window root ~/screenshot-`date +\"%F--%H-%M-%S\"`.png")
    , ((modMask,                 xK_Pause  ), spawn "gnome-screensaver-command --lock")
    , ((modMask,                 xK_Return ), dwmpromote)
    , ((modMask,                 xK_Right  ), withFocused (keysMoveWindow (20, 0)))
    , ((modMask,                 xK_Left   ), withFocused (keysMoveWindow (-20, 0)))
    , ((modMask,                 xK_Up     ), withFocused (keysMoveWindow (0, -20)))
    , ((modMask,                 xK_Down   ), withFocused (keysMoveWindow (0, 20)))
    , ((modMask .|. shiftMask,   xK_Right  ), withFocused (keysResizeWindow (20, 0) (0, 0)))
    , ((modMask .|. shiftMask,   xK_Left   ), withFocused (keysResizeWindow (-20, 0) (0, 0)))
    , ((modMask .|. shiftMask,   xK_Up     ), withFocused (keysResizeWindow (0, -20) (0, 0)))
    , ((modMask .|. shiftMask,   xK_Down   ), withFocused (keysResizeWindow (0, 20) (0, 0)))
    , ((modMask,                 xK_BackSpace), focusUrgent)
    , ((modMask,                 xK_F12    ), spawn "amixer -q set PCM 2dB+")
    , ((modMask,                 xK_F11    ), spawn "amixer -q set PCM 2dB-")
    ]
    ++
    [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
         | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]]


ownManageHook = composeAll . concat $
                [ [ className =? c --> doFloat | c <- floats]
                , [ className =? "Gecko" --> doF (W.shift "web") ]]
    where floats = ["MPlayer", "Gimp", "qiv", "Galculator", "Gcalctool"]


ownLayoutHook = smartBorders
                $ toggleLayouts (noBorders Full)
                $ Named "vert" tiled
                ||| Named "horiz" (Mirror tiled)
                ||| Named "tabbed" owntab
                ||| Named "accordion" Accordion
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
                , keys               = \c -> ownKeys c `M.union` keys defaultConfig c
                }

main = do statusbar <- spawnPipe statusBarCmd
          xmonad $ ownConfig statusbar

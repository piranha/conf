-- xmonad.hs
import Graphics.X11
import System.IO (hPutStrLn)
import qualified Data.Map as M
import Data.Bits ((.|.))
-- XMonad
import XMonad
import XMonad.Layouts
import XMonad.Operations
-- Extensions
import XMonad.Actions.WmiiActions
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Window


statusBarCmd= "dzen2 -e '' -w 800 -ta l -fg \"#a8a3f7\" -bg \"#3f3c6d\" -fn \"-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1\""

ownXPConfig :: XPConfig
ownXPConfig = defaultXPConfig 
              { font              = "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
              , bgColor           = "#3f3c6d"
              , fgColor           = "#a8a3f7"
              , fgHLight          = "#a8a3f7"
              , bgHLight          = "blue"
              , borderColor       = "#FFFFFF"
              }

tabbedConf :: TConf
tabbedConf = defaultTConf { fontName = "-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*-*" }

ownPP h = defaultPP
          { ppOutput = hPutStrLn h
--           , ppTitle  = (\x -> "")
          , ppSep    = " "
          , ppLayout = (\x -> "")
          }

ownKeys (XConfig {modMask = modm}) = 
    M.fromList $
         [ ((modm,                 xK_f      ), sendMessage ToggleLayout)
         , ((modm .|. controlMask, xK_x      ), xmonadPrompt ownXPConfig)
         , ((modm,                 xK_x      ), 
            submap . M.fromList $
                       [ ((0, xK_z), spawn "quodlibet --previous")
                       , ((0, xK_x), spawn "quodlibet --play-pause")
                       , ((0, xK_c), spawn "quodlibet --order=toggle")
                       , ((0, xK_v), spawn "quodlibet --next")
                       , ((0, xK_a), spawn "quodlibet --toggle-window")
                       ])
         , ((modm,                 xK_g      ), windowPromptGoto ownXPConfig)

-- currently unused because of already working keys :-)
--          , ((0,                    0x1008ff12), spawn "amixer -q set Master toggle")
--          , ((0,                    0x1008ff13), spawn "amixer -q set PCM 2dB+")
--          , ((0,                    0x1008ff11), spawn "amixer -q set PCM 2dB-")
         ]

ownConfig h = defaultConfig
                { borderWidth        = 1
                , normalBorderColor  = "#3f3c6d"
                , focusedBorderColor = "#FF0000"
                , terminal           = "urxvtc"
                , defaultGaps        = [(18,0,0,0), (18,0,0,0)]
                , modMask            = mod4Mask
                , logHook            = dynamicLogWithPP $ ownPP h
                , layoutHook         = Layout $ toggleLayouts (noBorders Full) $ tiled 
                                       ||| Mirror tiled
                                       ||| noBorders owntab
                , keys               = \c -> ownKeys c `M.union` keys defaultConfig c
                }
    where
      owntab = tabbed shrinkText tabbedConf
      tiled = Tall 1 0.03 0.5

main = do statusbar <- spawnPipe statusBarCmd
          xmonad $ ownConfig statusbar

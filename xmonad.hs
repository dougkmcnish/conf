import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

main :: IO ()
main =
  xmonad
  . ewmhFullscreen
  . ewmh
  . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
  $ myConfig

  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

     

myConfig = def
    { modMask    = mod4Mask -- rebind mod to Super
    , terminal = "alacritty"
    , layoutHook = myLayout
    , startupHook = do
        spawnOnce "emacs --daemon &"
        spawnOnce "xsetroot -cursor_name left_ptr &"
        spawnOnce "~/.fehbg &"
        spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --alpha 0 --tint 0x29293d --height 22 &"
    }
    `additionalKeysP`
    [ ("M-S-z"                   , spawn "i3lock -c 000000")
    , ("M-C-s"                   , unGrab *> spawn "scrot -s"        )
    , ("M-f"                     , spawn "qutebrowser"               )
    , ("M-d"                     , spawn "rofi -show drun"           )
    , ("M-S-d"                   , spawn "rofi -show run"            )
    , ("<XF86AudioRaiseVolume>"  , spawn "pamixer -i 5"              )
    , ("<XF86AudioLowerVolume>"  , spawn "pamixer -d 5"              )
    , ("<XF86AudioMute>"         , spawn "pamixer -t "               )
    , ("<XF86MonBrightnessUp>"   , spawn "light -A 5"                )
    , ("<XF86MonBrightnessDown>" , spawn "light -U 5"                )
    ]


myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
    where
      threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
      tiled    = Tall nmaster delta ratio      
      nmaster  = 1         -- default numer of windows in master pane
      ratio    = 1/2       -- proportion of master to screen
      delta    = 3/100     -- percent of screen to increment when resizing


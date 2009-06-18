import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.EZConfig(additionalKeys)

-- Importing Layouts, Combinators, etc.
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.DwmStyle
import XMonad.Layout.Circle
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators

myLayouts = named "dwmtiled" tiled ||| named "mirror" (Mirror tiled) ||| named "full" Full ||| named "circle" Circle ||| named "tabbed" simpleTabbedBottom
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

ignoreWindows = [ className =? "trayer" --> doIgnore
	, className =? "fbpanel" --> doIgnore
--	, className =? "stalonetray" --> doIgnore
	]

floatWindows = [ className =? "Xmessage" --> doFloat
	]

main = do
xmproc <- spawnPipe "xmobar /home/asenkovych/.xmonad/xmobar"

xmonad $ defaultConfig { 
	manageHook = composeAll [
		manageDocks <+> manageHook defaultConfig,
		className =? "stalonetray" --> doIgnore
		]
	, layoutHook = smartBorders $  avoidStruts $ myLayouts
	, logHook = dynamicLogWithPP $ xmobarPP { 
		ppOutput = hPutStrLn xmproc
		, ppTitle = xmobarColor "green" "" . shorten 50
	}

	, modMask = mod4Mask
      } `additionalKeys`
      [((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
       ((controlMask, xK_Print), spawn "scrot -s"),
       ((0, xK_Print), spawn "scrot"),
       ((mod4Mask, xK_d), sendMessage $ JumpToLayout "dwmtiled"),
       ((mod4Mask, xK_m), sendMessage $ JumpToLayout "mirror"),
       ((mod4Mask, xK_f), sendMessage $ JumpToLayout "full"),
       ((mod4Mask, xK_b), sendMessage $ JumpToLayout "tabbed"),
       ((mod4Mask, xK_c), sendMessage $ JumpToLayout "circle")
      ]

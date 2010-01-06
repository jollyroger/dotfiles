import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig(additionalKeys)

-- Pipes, environment variables, etc.
import XMonad.Util.Run(spawnPipe)
import System.IO
import System.Environment

-- Importing Layouts, Combinators, etc.
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.DwmStyle
import XMonad.Layout.Circle
import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators

myLayouts = named "dwmtiled" tiled ||| 
	named "mirror" (Mirror tiled) ||| 
	named "full" Full ||| 
	named "circle" Circle ||| 
	named "tabbed" simpleTabbedBottom
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

ignoreWindows = [ 
	className =? "trayer" --> doIgnore
	, className =? "fbpanel" --> doIgnore
	, className =? "stalonetray" --> doIgnore
	]

floatWindows = [ 
	className =? "Xmessage" --> doCenterFloat
	, className =? "Menu" --> doFloat
	, className =? "Qjackctl" --> doFloat
	, resource =? "qsynth" --> doFloat
	, title =? "glxgears" --> doFloat
	, title =? "ALSA Mixer" --> doFloat
	]

manageHooks = [
	manageDocks <+> manageHook defaultConfig
	]

myShortcuts = [
	((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
	, ((controlMask, xK_Print), spawn "scrot -s")
	, ((0, xK_Print), spawn "scrot")
	, ((mod4Mask, xK_d), sendMessage $ JumpToLayout "dwmtiled")
	, ((mod4Mask, xK_m), sendMessage $ JumpToLayout "mirror")
	, ((mod4Mask, xK_f), sendMessage $ JumpToLayout "full")
	, ((mod4Mask, xK_b), sendMessage $ JumpToLayout "tabbed")
	, ((mod4Mask, xK_c), sendMessage $ JumpToLayout "circle")
	]
	
myManageHook = composeAll ( concat [manageHooks, ignoreWindows, floatWindows] )
	
main = do
home <- getEnv "HOME"
xmproc <- spawnPipe ("xmobar")

xmonad $ defaultConfig { 
	terminal = "x-terminal-emulator"
	, manageHook = myManageHook
	, layoutHook = smartBorders $  avoidStruts $ myLayouts
	, logHook = dynamicLogWithPP $ xmobarPP { 
		ppOutput = hPutStrLn xmproc
		, ppTitle = xmobarColor "green" "" . shorten 50
	}

	, modMask = mod4Mask
      } `additionalKeys` myShortcuts

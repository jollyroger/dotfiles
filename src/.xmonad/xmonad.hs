import XMonad hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
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
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators

-- libraries for Java workaround
import XMonad.Hooks.SetWMName

gimpLayout = withIM (0.2) (Role "gimp-toolbox" ) $
    reflectHoriz $
    withIM (0.3) (Role "gimp-dock") Full

myLayouts = named "dwmtiled" tiled |||
        named "mirror" (Mirror tiled) |||
        named "full" Full |||
        named "circle" Circle |||
        named "tabbed" simpleTabbedBottom |||
    named "gimp" gimpLayout
    where
        tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

ignoreWindows = [ className =? "trayer" --> doIgnore
        , className =? "fbpanel"     --> doIgnore
        , className =? "stalonetray" --> doIgnore
        , className =? "Xfce4-notifyd" --> doIgnore
--        , className =? "Conky" --> doIgnore
        ]

floatWindows = [
        className =?   "Xmessage"   --> doCenterFloat
        , className =? "Menu"       --> doFloat
        , className =? "Qjackctl"   --> doFloat
        , className =? "virt-manager" --> doFloat
        , resource =?  "qsynth"     --> doFloat
        , title =?     "glxgears"   --> doFloat
        , title =?     "ALSA Mixer" --> doFloat
        , className =? "mpv"        --> doFloat
        ]

manageHooks = [
        manageDocks <+> manageHook defaultConfig
        ]

myShortcuts = [
        ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((mod4Mask, xK_Print), spawn "scrot")
        , ((0, xK_Print), spawn "scrot -s")
        , ((mod4Mask, xK_s), spawn "passmenu")
        , ((mod4Mask, xK_d), sendMessage $ JumpToLayout "dwmtiled")
        , ((mod4Mask, xK_m), sendMessage $ JumpToLayout "mirror")
        , ((mod4Mask, xK_f), sendMessage $ JumpToLayout "full")
        , ((mod4Mask, xK_b), sendMessage $ JumpToLayout "tabbed")
        , ((mod4Mask, xK_c), sendMessage $ JumpToLayout "circle")
        ]

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1:dev",
                 "2:web",
                 "3:adm",
                 "4:chat",
                 "5",
                 "6",
                 "7",
                 "8:music",
                 "9:tmp" ]

moveWindows = [ className =? "tkabber" --> doShift "4:chat"
        , className =? "gmpc" --> doShift "8:music"
        , className =? "sonata" --> doShift "8:music"
        ]

myHandleEventHook = mconcat
        [ docksEventHook
        , handleEventHook defaultConfig ]

myManageHook = composeAll ( concat [manageHooks, ignoreWindows, floatWindows, moveWindows] )

main = do
home <- getEnv "HOME"
xmproc <- spawnPipe ("xmobar")

xmonad $ withUrgencyHook NoUrgencyHook $ def {
        terminal = "/usr/bin/urxvt -e /usr/bin/tmux -2"
        , manageHook = myManageHook
        , startupHook = setWMName "LG3D"
        , layoutHook = smartBorders $  avoidStruts $ myLayouts
        , handleEventHook = myHandleEventHook
        , logHook = dynamicLogWithPP $ xmobarPP { 
                ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
        }
        , workspaces = myWorkspaces
        , modMask = mod4Mask
      } `additionalKeys` myShortcuts

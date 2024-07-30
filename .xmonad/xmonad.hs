-- Import necessary modules
import XMonad
import System.Exit (exitWith, ExitCode(ExitSuccess))
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat, isFullscreen, doFullFloat)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.CopyWindow (kill1)

import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Gaps (gaps, GapMessage(..), Direction2D(..))
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U), gaps, setGaps, GapMessage(DecGap, ToggleGaps, IncGap) )

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen (fullscreenManageHook, fullscreenSupport)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import Data.Monoid (Endo(..))  -- Import the Endo type
import XMonad.Hooks.ManageDocks
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import Data.Maybe (maybeToList)
import Control.Monad (join, when)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "brave"  -- Sets brave as browser

myEditor :: String
myEditor = "codium"  -- Sets VS Codium as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor = "#bc96da"

mySoundPlayer :: String
mySoundPlayer = "ffplay -nodisp -autoexit " -- The program that will play system sounds

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]  -- List of workspaces

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- Lock screen
    , ((modm,               xK_F1    ), spawn "betterlockscreen -l")

    -- Launch rofi and dashboard
    , ((modm,               xK_o     ), rofi_launcher)

    -- Launch side bar with Power Menu
--    , ((modm,		    xK_s     ), spawn "exec ~/.config/eww/powermenu")
     -- launch eww sidebar
    , ((modm,               xK_s     ), sidebarlaunch)
   , ((modm .|. shiftMask, xK_s     ), ewwclose)
    
	    -- Launch Tree Select
    , ((modm, xK_f), treeselectAction tsThemeConfig)
   
    -- Switch keyboard layout with Shift + Alt
    , ((shiftMask, xK_Alt_L), spawn "keyboard_layout_switch")

    -- Audio keys
    , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
 
    -- Screenshot
    , ((0,                    xK_Print), maimcopy)
    , ((modm,                 xK_Print), maimsave)

    -- Close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Gaps management
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,40), (D,60)]) -- reset the GapSpec
    
    , ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
    , ((modm .|. shiftMask, xK_t), sendMessage $ DecGap 10 L)                -- decrement the left-hand gap
    
    , ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
    , ((modm .|. shiftMask, xK_y), sendMessage $ DecGap 10 U)                -- decrement the top gap
    
    , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
    , ((modm .|. shiftMask, xK_u), sendMessage $ DecGap 10 D)                -- decrement the bottom gap

    , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
    , ((modm .|. shiftMask, xK_i), sendMessage $ DecGap 10 R)                -- decrement the right-hand gap

    -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm, xK_F1), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    -- Workspace switching and moving clients
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    -- Physical screen switching and moving clients
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

clipboardy :: MonadIO m => m ()
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"

-- Screenshot functions
maimcopy :: MonadIO m => m ()
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"

maimsave :: MonadIO m => m ()
maimsave = spawn "maim -s ~/Desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"

-- Rofi launcher
rofi_launcher :: MonadIO m => m ()
rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\""

sidebarlaunch :: MonadIO m => m ()
sidebarlaunch = spawn "exec ~/bin/eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"

ewwclose :: MonadIO m => m ()
ewwclose = spawn "exec ~/bin/eww close-all"

-- Layouts
myLayout = avoidStruts $ smartBorders $ gaps [(D, 10)] $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $ Tall 1 (3/100) (1/2) ||| Full

colorTrayer :: String
colorTrayer = "#FFFFFF"

-- Event hook
myEventHook = fullscreenEventHook

-- Startup hook
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "setxkbmap -layout us -variant altgr-intl"
    spawnOnce "nitrogen --restore"
    spawnOnce "picom --experimental-backends --config ~/.config/picom/picom.conf &"
    spawnOnce "nm-applet"
    spawnOnce "volumeicon"
    spawnOnce "blueman-applet"
    spawnOnce "flameshot"
    spawnOnce "greenclip daemon"
    spawnOnce "betterlockscreen -l"
   
    spawn "killall conky"   -- kill current conky on each restart
    spawn "killall trayer"  -- kill current trayer on each restart
    spawn "killall polybar" -- adding this in case of switching between xmobar and polybar.

-- Manage hook
myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = composeAll
  [ className =? "MPlayer"          --> doFloat
  , className =? "Gimp"             --> doFloat
  , className =? "PureRef"          --> doFloat 
  , className =? "discord"			--> doFloat
  , className =? "cool-retro-term"  --> doCenterFloat 
  , resource  =? "desktop_window"   --> doIgnore
  , resource  =? "kdesktop"         --> doIgnore
  , className =? "confirm"          --> doFloat
  , className =? "file_progress"    --> doFloat
  , className =? "dialog"           --> doFloat
  , className =? "download"         --> doFloat
  , className =? "error"            --> doFloat
  , className =? "notification"     --> doFloat
  , className =? "pinentry-gtk-2"   --> doFloat
  , className =? "splash"           --> doFloat
  , className =? "toolbar"          --> doFloat
  , className =? "Yad"              --> doCenterFloat
  , title =? "Oracle VM VirtualBox Manager"   --> doFloat
  , title =? "Order Chain - Market Snapshots" --> doFloat
  , title =? "Mozilla Firefox"      --> doShift (myWorkspaces !! 2)
  , className =? "Brave-browser"    --> doShift (myWorkspaces !! 2)
  , className =? "mpv"              --> doShift (myWorkspaces !! 7)
  , className =? "Gimp"             --> doShift (myWorkspaces !! 5)
  , className =? "VirtualBox Manager" --> doShift (myWorkspaces !! 9)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  ]

treeselectAction :: TS.TSConfig (X ()) -> X()
treeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "LibreOffice" "Launch LibreOffice applications" (return ()))
        [ Node (TS.TSNode "Writer"    "Launch LibreOffice Writer"    (spawn "libreoffice --writer")) []
        , Node (TS.TSNode "Calc"      "Launch LibreOffice Calc"      (spawn "libreoffice --calc")) []
        , Node (TS.TSNode "Impress"   "Launch LibreOffice Impress"   (spawn "libreoffice --impress")) []
        , Node (TS.TSNode "Draw"      "Launch LibreOffice Draw"      (spawn "libreoffice --draw")) []
        , Node (TS.TSNode "Base"      "Launch LibreOffice Base"      (spawn "libreoffice --base")) []
        , Node (TS.TSNode "Math"      "Launch LibreOffice Math"      (spawn "libreoffice --math")) []
        ]
    , Node (TS.TSNode "Development Tools" "Programs for development" (return ()))
        [ Node (TS.TSNode "Sublime Text"    "Launch Sublime Text"    (spawn "subl")) []
        , Node (TS.TSNode "Visual Studio Code" "Launch VS Code"      (spawn "codium")) []
        , Node (TS.TSNode "JupyterLab" "Launch JupyterLab" (spawn "bash ~/.local/bin/launch_jupyterlab.sh")) []
        , Node (TS.TSNode "Docker Desktop"  "Launch Docker Desktop"  (spawn "docker")) []
        , Node (TS.TSNode "GitKraken"       "Launch GitKraken"       (spawn "gitkraken")) []
        ]
    , Node (TS.TSNode "System Utilities" "Common system utilities" (return ()))
        [ Node (TS.TSNode "Terminal"         "Launch Terminal"           (spawn "xterm")) []
        , Node (TS.TSNode "File Manager"     "Launch File Manager"       (spawn "thunar")) []
        , Node (TS.TSNode "System Monitor"   "Launch System Monitor"     (spawn "gnome-system-monitor")) []
        , Node (TS.TSNode "Settings"         "Open System Settings"      (spawn "gnome-control-center")) []
        ]
    ]

tsThemeConfig :: TS.TSConfig a
tsThemeConfig = TS.TSConfig
    { TS.ts_hidechildren = True
    , TS.ts_background   = 0x1c1f26 
    , TS.ts_font         = "xft:FiraMono Nerd Font"
    , TS.ts_node         = (0xff000000, 0xff4e79a7) 
    , TS.ts_nodealt      = (0xff000000, 0xff4e79a7) 
    , TS.ts_highlight    = (0xffffffff, 0xff8c6d7d) 
    , TS.ts_extra        = 0xff000000 
    , TS.ts_node_width   = 200
    , TS.ts_node_height  = 30
    , TS.ts_originX      = 0
    , TS.ts_originY      = 0
    , TS.ts_indent       = 80
    , TS.ts_navigate     = myTreeNavigation
    }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape), TS.cancel)
    , ((0, xK_Return), TS.select)
    , ((0, xK_space),  TS.select)
    , ((0, xK_Up),     TS.movePrev)
    , ((0, xK_Down),   TS.moveNext)
    , ((0, xK_Left),   TS.moveParent)
    , ((0, xK_Right),  TS.moveChild)
    , ((0, xK_k),      TS.movePrev)
    , ((0, xK_j),      TS.moveNext)
    , ((0, xK_h),      TS.moveParent)
    , ((0, xK_l),      TS.moveChild)
    , ((0, xK_o),      TS.moveHistBack)
    , ((0, xK_i),      TS.moveHistForward)
    ]


-- Main function 
main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"  -- Launch xmobar
    xmonad $ docks $ ewmh def
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , keys = myKeys <+> keys def
        , mouseBindings = myMouseBindings
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook <+> docksEventHook <+> fullscreenEventHook
        , logHook            = dynamicLogWithPP xmobarPP
                                    { ppOutput = hPutStrLn xmproc
                                    , ppTitle  = xmobarColor "#c3e88d" "" . shorten 80
                                    }
        , startupHook        = myStartupHook

        }-- `additionalKeysP` myKeys
--		[ ((shiftMask .|. mod1Mask, xK_Shift_L), spawn "keyboard_layout_switch") ]
help :: String
help = "Your help message here..."

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad.Actions.CopyWindow
import System.Exit
import XMonad.Hooks.ManageHelpers

myModMask            = mod4Mask       -- changes the mod key to "super"
myFocusedBorderColor = "#ff0000"      -- color of focused border
myNormalBorderColor  = "#cccccc"      -- color of inactive border
myBorderWidth        = 1              -- width of border around windows
myTerminal           = "terminator"   -- which terminal software to use
myIMRosterTitle      = "Buddy List"   -- title of roster on IM workspace

myTitleColor     = "#eeeeee"  -- color of window title
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"

myWorkspaces =
  [
    "7:Chat",  "8:Dbg", "9:Pix",
    "4:Docs",  "5:Dev", "6:Web",
    "1:Term",  "2:Hub", "3:Mail",
    "0:VM",    "Extr1", "Extr2"
  ]

startupWorkspace = "5:Dev"  -- which workspace do you want to be on after launch?

defaultLayouts = smartBorders(avoidStruts(
  ResizableTall 1 (3/100) (1/2) []
  ||| noBorders Full))

hubLayout = ResizableTall 1 (3/100) (1/2) []
            ||| noBorders Full

chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)

gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

myLayouts =
  onWorkspace "2:Hub" hubLayout
  $ onWorkspace "7:Chat" chatLayout
  $ onWorkspace "9:Pix" gimpLayout
  $ defaultLayouts

myKeyBindings =
-- myKeyBindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_l), broadcastMessage ToggleStruts >> refresh)
--    , ((myModMask, xK_y), broadcastMessage NextLayout >> refresh) 
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask, xK_p), spawn "synapse")
    , ((myModMask .|. mod1Mask, xK_space), spawn "synapse")
    , ((myModMask, xK_u), focusUrgent)

    -- floating layer support
    , ((myModMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 10%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
    , ((myModMask, xK_v ), windows copyToAll) -- @@ Make focused window always visible
    , ((myModMask .|. shiftMask, xK_v ),  killAllOtherCopies) -- @@ Toggle window state back
  ]

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "synapse" --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "rdesktop" --> doFloat
  , (className =? "Komodo IDE") --> doF (W.shift "5:Dev")
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  , (className =? "Empathy") --> doF (W.shift "7:Chat")
  , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  , (className =? "Gimp") --> doF (W.shift "9:Pix")
  , (className =? "Pavucontrol") --> doF (W.shift "2:Hub")
  , (className =? "Gnome-terminal") --> doF (W.shift "1:Term")
  , (className =? "vlc" <&&> title =? "Playlist") --> doF (W.shift "2:Hub")
  , (className =? "vlc") --> doF copyToAll
  , (className =? "Gedit") --> doF (W.shift "4:Docs")
  , (title =? "KAlarm") --> doF (W.shift "2:Hub")
  , (title =? "Message — KAlarm") --> doF copyToAll <+> doRectFloat (W.RationalRect 0.7 0.04 0.25 0.25)
  ]

numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
    , xK_0, xK_minus, xK_equal
  ]

newKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [
    ((modMask,                 xK_b), sendMessage ToggleStruts)
    , ((modMask .|. shiftMask, xK_b), broadcastMessage ToggleStruts >> refresh)
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), do
      broadcastMessage NextLayout >> refresh
      setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window
    , ((modMask, xK_a), sendMessage MirrorShrink)
    , ((modMask, xK_z), sendMessage MirrorExpand)
    , ((modMask, xK_p), spawn "synapse")
    , ((modMask .|. mod1Mask, xK_space), spawn "synapse")
    , ((modMask, xK_u), focusUrgent)
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 10%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
    , ((modMask, xK_v ), windows copyToAll) -- @@ Make focused window always visible
    , ((modMask .|. shiftMask, xK_v ),  killAllOtherCopies) -- @@ Toggle window state back

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    ] 
    ++

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    --  Reset the layouts on the current workspace to default
--    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask , k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [
                     xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe,
                     xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla,
                     xK_agrave, xK_parenright, xK_equal
                    ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    myNumberKeys

myNumberKeys = 
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  M.toList (planeKeys myModMask (Lines 4) Finite) ++
  [
    ((m .|. myModMask, key), screenWorkspace sc
      >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ docks defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
--
  , keys = newKeys
  , workspaces = myWorkspaces
  , modMask = myModMask
  , handleEventHook = fullscreenEventHook
  , startupHook = do
      setWMName "LG3D"
      windows $ W.greedyView startupWorkspace
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook defaultConfig
      <+> composeAll myManagementHooks
      <+> manageDocks
  , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
        . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor myVisibleWSColor ""
        . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent = xmobarColor myUrgentWSColor ""
        . wrap myUrgentWSLeft myUrgentWSRight
    }
  }
--    `additionalKeys` newKeys

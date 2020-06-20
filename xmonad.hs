------------------------------------------------
--                                            --
--                Louis' Config               --
--                                            --
------------------------------------------------


----------------------------------------------------------------------------
-- Default IMPORTS
----------------------------------------------------------------------------
import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

----------------------------------------------------------------------------
-- My IMPORTS
----------------------------------------------------------------------------
  -- Allows to spawn xmonad
import XMonad.Util.Run(spawnPipe)
  -- Treats xmobar as a dock
import XMonad.Hooks.ManageDocks
  -- Send workspace and window info to xmobar
import XMonad.Hooks.DynamicLog
  -- Standard input output
import System.IO (Handle, hPutStrLn)
  -- DWMM like multi-monitor setup
import XMonad.Layout.IndependentScreens
  -- Add spacing between windows
import XMonad.Layout.Spacing
  -- Autospawn processes and programs
import XMonad.Util.SpawnOnce
  -- Allow java apps to function correctly
import XMonad.Hooks.SetWMName
  -- Allows cycling through workspaces
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
  -- Used when sending workspaces to workspace X
import Control.Monad
  -- Mouse follows focus
import XMonad.Actions.UpdatePointer

import XMonad.Layout.Gaps

----------------------------------------------------------------------------
-- Mouse focuses
----------------------------------------------------------------------------
  -- Whether focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

----------------------------------------------------------------------------
-- Keybindings
----------------------------------------------------------------------------
  -- Function to check if on the current screen
isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)
  -- Function to get the current screen ID
currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)
  -- Function to get the workspaces on the current screen
spacesOnCurrentScreen :: WSType
spacesOnCurrentScreen = WSIs (isOnScreen <$> currentScreen)
  --The keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Cycle through workspaces
    , ((modm, xK_Tab ), moveTo  Next spacesOnCurrentScreen)

    -- Move focused window to next/prev monitor
    , ((modm .|. shiftMask, xK_period), shiftNextScreen)
    , ((modm .|. shiftMask, xK_comma), shiftPrevScreen)

    -- Move focuse to next/prev monitor
    , ((modm, xK_period), nextScreen)
    , ((modm, xK_comma), prevScreen)

    , ((0, 0x1008ff11), spawn $ "amixer -q set Master 5%-")
    , ((0, 0x1008ff13), spawn $ "amixer -q set Master 5%+")
    , ((modm, xK_x), spawn $ "arcolinux-logout" )
    
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

   -- Move workspace to workspace N and focus 
    [((m .|. modm, k), windows $ onCurrentScreen f i)
      | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
      , (f, m) <- [(liftM2 (.) W.greedyView W.shift, modm .|. controlMask)]]
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce "/home/louis/.scripts/lefthand.sh &"
  spawnOnce "nitrogen --restore &"
  setWMName "LG3D &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
       -- Count screen and spawn that many xmobar on the screen
       nScreens    <- countScreens
       hs          <- mapM (spawnPipe . xmobarCommand) [0 .. nScreens-1]

       xmonad $ docks def {
      -- simple stuff
        terminal           = "alacritty",
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 2,
        modMask            = mod4Mask,
        workspaces         = withScreens nScreens (map show [1..5]),
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "orange",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout,
        startupHook        = myStartupHook,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = (mapM_ dynamicLogWithPP $ zipWith pp hs [0..nScreens]) >> updatePointer (0.5, 0.5) (0, 0)            
    }

  -- launch s many xmobar bars
xmobarCommand (S s) = unwords ["xmobar", "-x", show s, "/home/louis/.config/xmobar/xmobarrc"]

  -- the xmobar will pretty print with the following parameters
pp h s = marshallPP s defaultPP {
    ppCurrent           = color "green",
    ppVisible           = color "#004001",
    ppHiddenNoWindows   = color "#5c0000",
    ppHidden            = color "#0084ff",
    ppUrgent            = color "red",
    ppSep               = "   ",
    --ppOrder             = \(wss:layout:title:_) -> ["\NUL", title, "\NUL", wss],
    ppOutput            = hPutStrLn h
    }
    where color c = xmobarColor c ""


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
  -- Allows the key bindings to be easierly written
import XMonad.Util.EZConfig
  -- For fullscreen capabilities
import XMonad.Hooks.EwmhDesktops
  -- Modifies layouts
import XMonad.Layout.LayoutModifier
  -- Allows more customizable tiled layout
import XMonad.Layout.ResizableTile
  -- Allows there only to be certain number of windows be shown on a workspace
import XMonad.Layout.LimitWindows
  -- Change the layouts names
import XMonad.Layout.Renamed
  -- Make a layout increase the size of the window that has focus
import XMonad.Layout.Magnifier
  -- Basic floating layout but without the decoration
import XMonad.Layout.SimplestFloat
  -- Display without borders
import XMonad.Layout.NoBorders
  -- Allows youtube videos to be expanded properly
import XMonad.Hooks.ManageHelpers
  -- DWM style master/slave swapping
import XMonad.Actions.DwmPromote

import qualified XMonad.Hooks.DynamicBars as DynamicBars
import qualified XMonad.Hooks.DynamicLog as DynamicLog

import qualified XMonad.Actions.FlexibleResize as Flex

import XMonad.Layout.NoFrillsDecoration

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

  --The keybindings
myKeys = \c -> mkKeymap c $
        [ ("M-S-<Return>", spawn myTerminal)
        -- launch dmenu
        , ("M-p", spawn "dmenu_run")

	-- launch flameshot
	, ("M-S-s", spawn "flameshot gui")

        -- close focused window
        , ("M-S-c", kill)

        -- Rotate through the available layout algorithms
        , ("M-<Space>", sendMessage NextLayout)

        -- Resize viewed windows to the correct size
        , ("M-n", refresh)

        -- Move focus to the next window
        , ("M-j", windows W.focusDown)

        -- Move focus to the previous window
        , ("M-k", windows W.focusUp  )

        -- Move focus to the master window
        , ("M-m", windows W.focusMaster  )

        -- Swap the focused window and the master window
        , ("M-<Return>", dwmpromote)

        -- Swap the focused window with the next window
        , ("M-S-j", windows W.swapDown  )

        -- Swap the focused window with the previous window
        , ("M-S-k", windows W.swapUp    )

        -- Shrink the master area
        , ("M-h", sendMessage Shrink)

        -- Expand the master area
        , ("M-l", sendMessage Expand)

        -- Push window back into tiling
        , ("M-t", withFocused $ windows . W.sink)

        -- Cycle through workspaces
        , ("M-,", moveTo Prev HiddenWS)
        , ("M-.", moveTo Next HiddenWS)

        -- Move focused window to next/prev monitor
        , ("M-S-.", shiftNextScreen)
        , ("M-S-,", shiftPrevScreen)

        -- Move focuse to next/prev monitor
        , ("M-o", nextScreen)
        , ("M-i", prevScreen)

        , ("M-<Tab>", swapNextScreen)

        -- Increase and decrease window and screen spacing
        , ("M-S-[", decScreenWindowSpacing 2)
        , ("M-S-]", incScreenWindowSpacing 2)

        -- Set window and screen spacing back to default
        , ("M-=", setScreenWindowSpacing 10)

        , ("<XF86AudioLowerVolume>", spawn $ "amixer -q set Master 5%-")
        , ("<XF86AudioRaiseVolume>", spawn $ "amixer -q set Master 5%+")

        , ("M-b", sendMessage ToggleStruts)
        
        -- Quit xmonad
        , ("M-S-q", spawn  "killall xinit")

        -- Restart xmonad
        , ("M-q", spawn "killall xmobar; killall trayer;  xmonad --recompile; xmonad --restart")
        ]
        
        ++

        [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]
        

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
------------------------------------------------------------------------

-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 10
           $ ResizableTall 1 (3/100) (1/2) []

magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 10
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full 

myLayout = avoidStruts (tall ||| magnify ||| noBorders monocle) ||| noBorders Full

------------------------------------------------------------------------
-- Manage Hook:
------------------------------------------------------------------------
myManageHook = composeAll
   [  className =? "MPlayer"        --> doFloat
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
-- myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup Hook:
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "$HOME/.xmonad/scripts/lefthand.sh &"
  spawnOnce "feh ~/.xmonad/wallpaper/hawk.jpg feh --bg-center ~/.xmonad/wallpaper/bear.jpg &"
  spawnOnce "$HOME/.xmonad/scripts/screen_layout.sh &"
  spawnOnce "flameshot &"
  spawnOnce "picom -CGb &"
  setWMName "LG3D &"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "inkscape-figures watch &"
  spawnOnce "dunst &"
  spawnOnce "sxhkd &"
  DynamicBars.dynStatusBarStartup barCreator barDestroyer
  ewmhDesktopsStartup 

------------------------------------------------------------------------
-- Main:
------------------------------------------------------------------------

myTerminal = "alacritty"

main = do

       xmonad $ ewmh def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 2,
        modMask            = mod1Mask,
        workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
        normalBorderColor  = "#12253D",
        focusedBorderColor = "#ffd36b",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        startupHook        = myStartupHook,
        manageHook         = myManageHook <+> manageDocks,
      -- fullscreenhook is used to allow youtube videos to take fullscreen when double clicking
        handleEventHook    = docksEventHook <+> fullscreenEventHook <+> DynamicBars.dynStatusBarEventHook barCreator barDestroyer,
        logHook = DynamicBars.multiPP myLogPPActive myLogPP 
   
    }
   -- `additionalKeysP` myKeys 

------------------------------------------------------------------------
-- Status bars
------------------------------------------------------------------------

myLogPP :: DynamicLog.PP
myLogPP = DynamicLog.defaultPP
  { DynamicLog.ppCurrent = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap "<fc=#81d0f7>[</fc>" "<fc=#81d0f7>]</fc>" -- focused monitor and focused workspace
  , DynamicLog.ppVisible = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap "<fc=#81d0f7>'</fc>" "<fc=#81d0f7>'</fc>" -- shows the workspace the unfocused monitor is displaying on the focused monitor
  , DynamicLog.ppHidden  = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap " " " " -- hidden workspaces but with windows in it
  , DynamicLog.ppHiddenNoWindows = DynamicLog.xmobarColor "#FF0000" "" . DynamicLog.wrap " " " "
 -- , DynamicLog.ppUrgent  = ""
  , DynamicLog.ppTitle   = DynamicLog.xmobarColor "#9d9fa1" "" . DynamicLog.wrap " " " " . DynamicLog.shorten 50
  , DynamicLog.ppLayout  = DynamicLog.xmobarColor "#d7a3f7" "" . DynamicLog.wrap " " " "
  , DynamicLog.ppSep     = " "
  }

myLogPPActive :: DynamicLog.PP
myLogPPActive = DynamicLog.defaultPP
  { DynamicLog.ppCurrent = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap "<fc=#81d0f7>[</fc>" "<fc=#81d0f7>]</fc>" -- focused monitor and focused workspace
  , DynamicLog.ppVisible = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap "<fc=#81d0f7>'</fc>" "<fc=#81d0f7>'</fc>" -- shows the workspace the unfocused monitor is displaying on the focused monitor
  , DynamicLog.ppHidden  = DynamicLog.xmobarColor "#56bf64" "" . DynamicLog.wrap " " " " -- hidden workspaces but with windows in it
  , DynamicLog.ppHiddenNoWindows = DynamicLog.xmobarColor "#FF0000" "" . DynamicLog.wrap " " " "
 -- , DynamicLog.ppUrgent  = ""
  , DynamicLog.ppTitle   = DynamicLog.xmobarColor "#c0e66e" "" . DynamicLog.wrap " " " " . DynamicLog.shorten 50
  , DynamicLog.ppLayout  = DynamicLog.xmobarColor "#d7a3f7" "" . DynamicLog.wrap " " " "
  , DynamicLog.ppSep     = " "
  }

barCreator :: DynamicBars.DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid ++ " $HOME/.config/xmobar/xmobarrc"

barDestroyer :: DynamicBars.DynamicStatusBarCleanup
barDestroyer = return ()


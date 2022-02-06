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
  -- Make sure the cursor follows the screen
import XMonad.Actions.Warp
  -- Change the default config
import XMonad.Config.Desktop
  -- Resize window with mouse from any corner
import qualified XMonad.Actions.FlexibleResize as Flex
  -- Create the dynamic logs for the pretty printer 
import XMonad.Hooks.StatusBar.PP as DynamicLog

  -- MultiPP libraries
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Graphics.X11.Xinerama
import XMonad.Prelude

  -- Add swallowing
import XMonad.Hooks.WindowSwallowing

  -- Add nofrills decoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimpleDecoration

  -- Add sub layouts
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Accordion
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.BoringWindows

import XMonad.Actions.Navigation2D

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
         
        [("M-S-<Return>", spawn myTerminal)
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

        -- Move focus to the next window (boring so windows sub layouts dont get focused)
        , ("M-j", XMonad.Layout.BoringWindows.focusDown)

        -- Move focus to the previous window (bording so windows sub layouts dont get focused)
        , ("M-k", XMonad.Layout.BoringWindows.focusUp)

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
        , ("M-,", moveTo Prev hiddenWS)
        , ("M-.", moveTo Next hiddenWS)

        -- Move focused window to next/prev monitor
        , ("M-S-.", windowToScreen R True)
        , ("M-S-,", windowToScreen L True)

        -- Move focuse to next/prev monitor
        , ("M-o", nextScreen <+> banishScreen LowerLeft)
        , ("M-i", prevScreen <+> banishScreen LowerLeft)

        , ("M-<Tab>", swapNextScreen)

        -- Increase and decrease window and screen spacing
        , ("M-S-[", decScreenWindowSpacing 2) , ("M-S-]", incScreenWindowSpacing 2)
        -- Set window and screen spacing back to default
        , ("M-=", setScreenWindowSpacing 10)

        , ("<XF86AudioLowerVolume>", spawn $ "amixer -D pulse sset Master 5%-")
        , ("<XF86AudioRaiseVolume>", spawn $ "amixer -D pulse sset Master 5%+")

        , ("M-b", sendMessage ToggleStruts)

	-- Sub Layouts
	, ("M4-h", sendMessage $ pullGroup L)
	, ("M4-l", sendMessage $ pullGroup R)
	, ("M4-k", sendMessage $ pullGroup U)
	, ("M4-j", sendMessage $ pushGroup D)


	, ("M4-m", withFocused (sendMessage . MergeAll))
	, ("M4-u", withFocused (sendMessage . UnMerge))

	, ("M4-i", onGroup W.focusUp')
	, ("M4-o", onGroup W.focusDown')

	, ("M4-<Space>", toSubl NextLayout)
        
        -- Quit xmonad
        , ("M-S-q", spawn  "killall xinit")

        -- Restart xmonad
        , ("M-q", spawn "killall polybar; rm -R /home/louis/.xmonad;  xmonad --recompile; xmonad --restart")
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
--
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

active = "#268bd2"
inactive = "#082030"

topBarTheme = def
    { inactiveBorderColor   = inactive
    , inactiveColor         = inactive
    , inactiveTextColor     = inactive
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , decoHeight            = 10
    }

myTabTheme = def
    { activeColor           = active
    , inactiveColor         = inactive
    , activeBorderColor     = active
    , inactiveBorderColor   = inactive
    , activeTextColor       = "#ffffff"
    , inactiveTextColor     = "#8f8f8f"
    }

addTopBar = noFrillsDeco shrinkText topBarTheme

tall     = renamed [XMonad.Layout.Renamed.Replace "tall"]
	   $ windowNavigation
	   $ addTopBar
	   $ addTabs shrinkText myTabTheme
	   $ subLayout [] (Simplest ||| Accordion)
	   $ boringWindows
           $ limitWindows 12
           $ mySpacing 10
           $ ResizableTall 1 (3/100) (1/2) []

magnify  = renamed [XMonad.Layout.Renamed.Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 10
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [XMonad.Layout.Renamed.Replace "monocle"]
	   $ boringWindows
           $ mySpacing 10
           $ limitWindows 20 Full 

full     = renamed [XMonad.Layout.Renamed.Replace "full"]
	   $ boringWindows
	   $ noBorders Full

myLayout = avoidStruts (tall ||| monocle) ||| full


------------------------------------------------------------------------
-- Manage Hook:
------------------------------------------------------------------------
myManageHook = composeAll
   [  className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "pwrworld.exe"   --> doFloat
    , resource  =? "desktop_window" --> doFloat
    , resource  =? "dialog"         --> doFloat
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
  spawnOnce "$HOME/.config/xmonad/scripts/lefthand.sh &"
  spawnOnce "feh ~/.config/xmonad/wallpaper/flamingo2.jpg feh --bg-center ~/.config/xmonad/wallpaper/hawk.jpg &"
  spawnOnce "$HOME/.config/xmonad/scripts/screen_layout.sh &"
  spawnOnce "flameshot &"
  spawnOnce "picom -CGb &"
  setWMName "compiz &"
  spawnOnce "xsetroot -cursor_name left_ptr &"
  spawnOnce "nm-applet &"
  spawnOnce "kmonad $HOME/.config/kmonad/myConfig.kbd &"

------------------------------------------------------------------------
-- Main:
------------------------------------------------------------------------

myTerminal = "alacritty"

-- Setup the polybar FIFO for each screen
polybarFIFO :: Int -> IO Handle
polybarFIFO id = spawnPipe $ "$HOME/.config/xmonad/scripts/polybarFifo.sh " <> show (id+1)

main = do
        -- Create the polybar on each screen
        xmproc <- spawnPipe "$HOME/.config/xmonad/scripts/polybar/polybar_start.sh"

        xmonad . ewmhFullscreen . ewmh . docks . myConfig =<< mapM polybarFIFO =<< getScreens

myConfig hs = desktopConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = 2,
        modMask            = mod1Mask,
        workspaces         = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],

        normalBorderColor  = inactive,
        focusedBorderColor = "#3dafff",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = lessBorders OnlyScreenFloat $ myLayout,
        startupHook        = myStartupHook,
        manageHook         = myManageHook,
        handleEventHook    = swallowEventHook (className =? "Alacritty") (return True), 
        logHook = multiPP myLogPPPolybar myLogPPActivePolybar hs

   
    }

-------------------------------
-- Polybar status bar
-------------------------------
myLogPPPolybar :: DynamicLog.PP
myLogPPPolybar = def
  { DynamicLog.ppCurrent = DynamicLog.wrap "%{F#81d0f7}[%{F-}%{F#56bf64}" "%{F-}%{F#81d0f7}]%{F-}" -- focused monitor and focused workspace
  , DynamicLog.ppVisible = DynamicLog.wrap "%{F#81d0f7}'%{F-}%{F#56bf64}" "%{F-}%{F#81d0f7}'%{F-}" -- shows the workspace the unfocused monitor is displaying on the focused monitor
  , DynamicLog.ppHidden  = DynamicLog.wrap "%{F#56bf64} " " %{F-}" -- hidden workspaces but with windows in it
  , DynamicLog.ppHiddenNoWindows = DynamicLog.wrap " %{F#FF0000}" "%{F-} "
 -- , DynamicLog.ppUrgent  = ""
  , DynamicLog.ppTitle   = DynamicLog.wrap " %{F#c0e66e}" "%{F-}" . DynamicLog.shorten 45
  , DynamicLog.ppLayout  = DynamicLog.wrap " %{F#d7a3f7}" "%{F-} "
  , DynamicLog.ppSep     = " "
  }

myLogPPActivePolybar :: DynamicLog.PP
myLogPPActivePolybar = def
  { DynamicLog.ppCurrent = DynamicLog.wrap "%{F#81d0f7}[%{F-}%{F#56bf64}" "%{F-}%{F#81d0f7}]%{F-}" -- focused monitor and focused workspace
  , DynamicLog.ppVisible = DynamicLog.wrap "%{F#81d0f7}'%{F-}%{F#56bf64}" "%{F-}%{F#81d0f7}'%{F-}" -- shows the workspace the unfocused monitor is displaying on the focused monitor
  , DynamicLog.ppHidden  = DynamicLog.wrap "%{F#56bf64} " " %{F-}" -- hidden workspaces but with windows in it
  , DynamicLog.ppHiddenNoWindows = DynamicLog.wrap "%{F#FF0000} " " %{F-}"
 -- , DynamicLog.ppUrgent  = ""
  , DynamicLog.ppTitle   = DynamicLog.wrap " %{F#b8b8b8}" "%{F-} " . DynamicLog.shorten 45
  , DynamicLog.ppLayout  = DynamicLog.wrap " %{F#d7a3f7}" "%{F-} "
  , DynamicLog.ppSep     = " "
  }

-------------------------------
-- multiPP funciton
-------------------------------
multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> [Handle] -- ^ Handles for the status bars, in order of increasing X
                    -- screen number
        -> X ()
multiPP = multiPP' dynamicLogString

multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
    state <- get
    let pickPP :: WorkspaceId -> WriterT (Last XState) X String
        pickPP ws = do
            let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset state
            put state{ windowset = W.view ws $ windowset state }
            out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
            when isFoc $ get >>= tell . Last . Just
            return out
    traverse put . getLast
        =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
        =<< mapM screenWorkspace (zipWith const [0..] handles)
    return ()

getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

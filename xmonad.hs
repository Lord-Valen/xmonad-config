-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap, checkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-- doom-one
{-
colorBack = "#282c34" --bg
colorFore = "#bbc2cf" --fg
color0 = "#1c1f24" --base1
color1 = "#ff6c6b" --red
color2 = "#98be65" --green
color3 = "#da8548" --orange
color4 = "#51afef" --blue
color5 = "#c678dd" --magenta
color6 = "#5699af" --dark-cyan
color7 = "#202328" --base2
color8 = "#5b6268" --base5
color9 = "#da8548" --orange
color10 = "#4db5bd" --teal
color11 = "#ecbe7b" --yellow
color12 = "#2257a0" --dark-blue
color13 = "#a9a1e1" --violet
color14 = "#46d9ff" --cyan
color15 = "#dfdfdf" --base8
-}
-- doom-outrun-electric
colorBack = "#0c0a20" --bg
colorFore = "#f2f3f7" --fg
color0 = "#1f1147" --base1
color1 = "#e61f44" --red
color2 = "#a7da1e" --green
color3 = "#cf433e" --orange
color4 = "#1ea8fc" --blue
color5 = "#ff2afc" --magenta
color6 = "#204052" --dark-cyan
color7 = "#110d26" --base2
color8 = "#ba45a3" --base5
color9 = "#cf433e" --orange
color10 = "#a875ff" --teal
color11 = "#ffd400" --yellow
color12 = "#3f88ad" --dark-blue
color13 = "#df85ff" --violet
color14 = "#42c6ff" --cyan
color15 = "#919ad9" --base8

myFont :: String
myFont = "xft:Inconsolata Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myRunPrompt :: String
myRunPrompt = "rofi -show run"

myTerminal :: String
myTerminal = "kitty"

myBrowser :: String
myBrowser = "brave "

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = colorBack -- Border color of normal windows

myFocusColor :: String
myFocusColor  = color14 -- Border color of focused windows

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

colorTrayer :: String
colorTrayer = "--tint " ++ "0x" ++ tail colorBack

-- Status Bar
myPP = def { ppCurrent = xmobarColor color5 ""
                         . wrap ("<box type=Bottom width=2 mb=2 color=" ++ color5 ++ ">") "</box>"
             -- Visible but not current workspace
           , ppVisible = xmobarColor color5 ""
             -- Hidden workspace
           , ppHidden = xmobarColor color4 ""
                        . wrap ("<box type=Top width=2 mt=2 color=" ++ color4 ++ ">") "</box>"
             -- Hidden workspaces (no windows)
           , ppHiddenNoWindows = xmobarColor color4 ""
             -- Title of active window
           , ppTitle = xmobarColor color15 "" . shorten 60
             -- Separator character
           , ppSep = "<fc=" ++ color8 ++ "> <fn=1>|</fn> </fc>"
             -- Urgent workspace
           , ppUrgent = xmobarColor color1 "" . wrap "!" "!"
             -- Adding # of windows on current workspace to the bar
           , ppExtras = [windowCount]
             -- order of things in xmobar
           , ppOrder = \(ws:l:t:ex) -> [ws,l] ++ ex ++ [t]
           }

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure $ statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc" (pure myPP)
barSpawner 1 = pure $ statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc" (pure myPP)
barSpawner 2 = pure $ statusBarProp "xmobar -x 2 ~/.config/xmobar/xmobarrc" (pure myPP)

myStartupHook :: X ()
myStartupHook = do
  return () >> checkKeymap myConfig myKeymap
  spawnOnce "lxsession &"
  spawnOnce "picom &"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "nm-applet &"
  spawnOnce "volumeicon &"
  spawnOnce $ "killall trayer; trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor primary --transparent true --alpha 0 " ++ colorTrayer ++ " --height 30 &"
  spawnOnce "feh --no-fehbg --bg-fill -z ~/.config/wallpaper/*"  -- feh set random wallpaper

myScratchPads :: [NamedScratchpad]
myScratchPads = [
  NS "terminal" spawnTerm findTerm manageTerm,
  NS "calculator" spawnCalc findCalc manageCalc
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad";
      findTerm = title =? "scratchpad";
    manageTerm = customFloating $ W.RationalRect l t w h
      where l = 0.95 -w; t = 0.95 -h; w = 0.9; h = 0.9
    spawnCalc = "qalculate-gtk";
      findCalc = className =? "Qalculate-gtk";
    manageCalc = customFloating $ W.RationalRect l t w h
      where l = 0.7 -w; t = 0.75 -h; w = 0.4; h = 0.5;

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 6
           $ mySpacing' 6
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing' 6
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
                -- I cannot add spacing to this layout because it will add spacing between window and tabs which looks bad.
       $ tabbed shrinkText myTabTheme

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def {
  fontName = myFont,
  activeColor         = color14,
  inactiveColor       = colorBack,
  activeBorderColor   = color14,
  inactiveBorderColor = colorBack,
  activeTextColor     = colorBack,
  inactiveTextColor   = colorFore
  }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def {
  swn_font    = "xft:Ubuntu:bold:size=60",
  swn_fade    = 1.0,
  swn_bgcolor = "#1c1f24",
  swn_color   = "#ffffff"
  }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                      ||| noBorders monocle
                      ||| noBorders tabs
                      ||| grid
                      ||| threeCol
                      ||| threeRow

myWorkspaces = [" sys ", " doc ", " www ", " dev ", " cht ", " vms ", " mus ", " vid ", " gfx "]

myManageHook = composeAll [
  className =? "confirm"        --> doFloat,
  className =? "file_progress"  --> doFloat,
  className =? "dialog"         --> doFloat,
  className =? "download"       --> doFloat,
  className =? "error"          --> doFloat,
  className =? "notification"   --> doFloat,
  className =? "pinentry-gtk-2" --> doFloat,
  className =? "splash"         --> doFloat,
  className =? "toolbar"        --> doFloat,
  className =? "Yad"            --> doCenterFloat,
  className =? "Zotero"         --> doShift ( myWorkspaces !! 1 ),
  className =? "Brave-browser"  --> doShift ( myWorkspaces !! 2 ),
  className =? "Ferdi"          --> doShift ( myWorkspaces !! 4 ),
  className =? "Element"        --> doShift ( myWorkspaces !! 4 ),
  className =? "Signal"         --> doShift ( myWorkspaces !! 4 ),
  className =? "zoom"           --> doShift ( myWorkspaces !! 4 ),
  className =? "Virt-manager"   --> doShift ( myWorkspaces !! 5 ),
  className =? "mpv"            --> doShift ( myWorkspaces !! 7 ),
  className =? "Steam"          --> doShift ( myWorkspaces !! 8 ),
  className =? "Lutris"         --> doShift ( myWorkspaces !! 8 ),
  className =? "itch"           --> doShift ( myWorkspaces !! 8 ),
  className =? "Gimp"           --> doShift ( myWorkspaces !! 8 ),
  className =? "Inkscape"       --> doShift ( myWorkspaces !! 8 ),
  isFullscreen                  --> doFullFloat
  ] <+> namedScratchpadManageHook myScratchPads

-- START_KEYS
myKeymap :: [(String, X ())]
myKeymap = [
  -- KB_GROUP Xmonad
  ("M-C-r", spawn "xmonad --recompile"),
  ("M-S-r", spawn "xmonad --restart"),
  ("M-S-x", io exitSuccess),

  -- KB_GROUP Run Prompt
  ("M-S-<Return>", spawn myRunPrompt),
  ("M-C-<Return>", spawn myRunPrompt), -- Workaround for when M-S-<Return> won't register <Return>

  -- KB_GROUP Commonly used programs
  ("M-<Return>", spawn myTerminal),
  ("M-b", spawn myBrowser),
  ("M-M1-h", spawn (myTerminal ++ " -e htop")),

  -- KB_GROUP Kill windows
  ("M-S-q", kill1),     -- Kill the currently focused client
  ("M-S-c", killAll),   -- Kill all windows on current workspace

  -- KB_GROUP Workspaces
  ("M-.", nextScreen),  -- Switch focus to next monitor
  ("M-,", prevScreen),  -- Switch focus to prev monitor
  ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP),       -- Shifts focused window to next ws
  ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP),  -- Shifts focused window to prev ws

  -- KB_GROUP Floating windows
  ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
  ("M-S-t", sinkAll),                       -- Push ALL floating windows to tile

  -- KB_GROUP Increase/decrease spacing (gaps)
  ("C-M1-m", decScreenSpacing 2),         -- Decrease screen spacing
  ("C-M1-n", decWindowSpacing 2),         -- Decrease window spacing
  ("C-M1-e", incWindowSpacing 2),         -- Increase window spacing
  ("C-M1-i", incScreenSpacing 2),         -- Increase screen spacing

  -- KB_GROUP Windows navigation
  ("M-m", windows W.focusMaster),  -- Move focus to the master window
  ("M-n", windows W.focusDown),    -- Move focus to the next window
  ("M-e", windows W.focusUp),      -- Move focus to the prev window
  ("M-i", windows W.swapMaster), -- Swap the focused window and the master window
  ("M-S-n", windows W.swapDown),   -- Swap focused window with next window
  ("M-S-e", windows W.swapUp),     -- Swap focused window with prev window
  ("M-<Backspace>", promote),      -- Moves focused window to master, others maintain order
  ("M-S-<Tab>", rotSlavesDown),    -- Rotate all windows except master and keep focus in place
  ("M-C-<Tab>", rotAllDown),       -- Rotate all the windows in the current stack

  -- KB_GROUP Layouts
  ("M-<Tab>", sendMessage NextLayout),           -- Switch to next layout
  ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full

  -- KB_GROUP Increase/decrease windows in the master pane or the stack
  ("M-S-<Up>", sendMessage (IncMasterN 1)),      -- Increase # of clients master pane
  ("M-S-<Down>", sendMessage (IncMasterN (-1))), -- Decrease # of clients master pane
  ("M-C-<Up>", increaseLimit),                   -- Increase # of windows
  ("M-C-<Down>", decreaseLimit),                 -- Decrease # of windows

  -- KB_GROUP Window resizing
  ("M-h", sendMessage Shrink),                   -- Shrink horiz window width
  ("M-l", sendMessage Expand),                   -- Expand horiz window width
  ("M-M1-n", sendMessage MirrorShrink),          -- Shrink vert window width
  ("M-M1-e", sendMessage MirrorExpand),          -- Expand vert window width

  -- KB_GROUP Sublayouts
  -- This is used to push windows to tabbed sublayouts, or pull them out of it.
  ("M-C-m", sendMessage $ pullGroup L),
  ("M-C-n", sendMessage $ pullGroup R),
  ("M-C-e", sendMessage $ pullGroup U),
  ("M-C-i", sendMessage $ pullGroup D),
  ("M-C-?", withFocused (sendMessage . MergeAll)),
  ("M-C-/", withFocused (sendMessage . UnMergeAll)),
  ("M-C-.", onGroup W.focusUp'),    -- Switch focus to next tab
  ("M-C-,", onGroup W.focusDown'),  -- Switch focus to prev tab

  -- KB_GROUP Scratchpads
  -- Toggle show/hide these programs.  They run on a hidden workspace.
  -- When you toggle them to show, it brings them to your current workspace.
  -- Toggle them to hide and it sends them back to hidden workspace (NSP).
  ("M-s t", namedScratchpadAction myScratchPads "terminal"),
  ("M-s c", namedScratchpadAction myScratchPads "calculator"),

  -- KB_GROUP Emacs
  ("M-a a", spawn myEmacs), -- emacs
  ("M-a e", spawn (myEmacs ++ ("--eval '(eshell)'"))), -- eshell
  ("M-a f", spawn (myEmacs ++ ("--eval '(elfeed)'"))), -- elfeed
  ("M-a w", spawn (myEmacs ++ ("--eval '(eww)'"))), -- emacs web wowser
  ("M-a i", spawn (myEmacs ++ ("--eval '(circe)'"))), -- emacs irc client

  -- KB_GROUP XF86
  ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
  ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  ]
  -- The following lines are needed for named scratchpads.
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"));
    nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
-- END_KEYS

myConfig = def {
    manageHook = myManageHook <+> manageDocks,
    --keys = \c -> mkKeymap c myKeymap,
    modMask = myModMask,
    terminal = myTerminal,
    startupHook = myStartupHook,
    layoutHook = showWName' myShowWNameTheme myLayoutHook,
    workspaces = myWorkspaces,
    borderWidth = myBorderWidth,
    normalBorderColor = myNormColor,
    focusedBorderColor = myFocusColor
    } `additionalKeysP` myKeymap

main :: IO ()
main = xmonad $ dynamicSBs barSpawner . ewmh . docks $ myConfig

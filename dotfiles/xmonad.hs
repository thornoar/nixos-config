-- Base
import XMonad
import qualified XMonad.StackSet as W
import System.Exit

-- Monads
import Control.Monad (void)

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll (sinkAll, killAll, withAll)
import XMonad.Actions.Minimize
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Volume
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Actions.GroupNavigation

-- Data
import Data.Char ( isSpace, toUpper, isSpace )
import qualified Data.Map as M
import Data.List
import Data.Ratio

-- Hooks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.RefocusLast

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Simplest
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Reflect
-- import XMonad.Layout.Master
-- import XMonad.Layout.AutoMaster
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (Toggle(..), mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Magnifier as MG
import qualified XMonad.Layout.BoringWindows as BW

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch
import Control.Arrow (first)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, remapKeysP)
import XMonad.Util.NamedWindows
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook

-- Xmobar
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import Colors
import Size
import Misc
windowSpace' :: Int
windowSpace' = windowSpace `div` 2
myFloatingRectangle :: W.RationalRect
myFloatingRectangle = W.RationalRect ((1 - scratchpadWidth) / 2) ((1 - scratchpadHeight) / 2) scratchpadWidth scratchpadHeight

grid = named "Grid"
  $ spacingWithEdge windowSpace'
  $ MG.magnifierczOff magnifiedScale
  $ Grid (16/9)

tallAccordion = named "Master"
  $ spacingWithEdge windowSpace'
  $ mkToggle (single MIRROR)
  $ MG.magnifierczOff magnifiedScale
  -- $ autoMaster 1 (3/100) Accordion
  -- $ mastered (3/100) (1/2) Accordion
  $ reflectHoriz
  $ combineTwo (TwoPane (3/100) (1/2)) Accordion (Grid (16/9))

accordion = named "Accordion"
  $ spacingWithEdge windowSpace'
  $ mkToggle (single MIRROR)
  $ Mirror Accordion

tabs = named "Tabbed"
  $ spacing windowSpace'
  $ tabbedAlways shrinkText myTabTheme

myLayout = grid ||| tallAccordion ||| accordion ||| tabs ||| Full

ffmap :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
ffmap f ma mb = ma >>= (\x -> fmap (f x) mb)

myTerminal :: String
myTerminal = "$TERMINAL"

myBrowser :: String
myBrowser = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask :: KeyMask
myModMask = mod4Mask

myStartupHook :: X ()
myStartupHook = do
  spawn setWallpaperCmd
  -- spawn "xmodmap ~/.Xmodmap"

myWorkspaces :: [String]
myWorkspaces = ["fst", "snd", "trd", "fth", "aux"]

myFont :: String
myFont = "xft:Hack Mono:mono:size=21:bold=false:antialias=true:hinting=true"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

archwiki, nixoswiki, reddit, libgen :: S.SearchEngine
archwiki    = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
nixoswiki   = S.searchEngine "nixoswiki" "https://nixos.wiki/index.php?search="
reddit      = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
libgen      = S.searchEngine "libgen" "https://www.libgen.is/search.php?req="

searchList :: [(String, S.SearchEngine)]
searchList = [
    ("a", archwiki),
    ("n", nixoswiki),
    ("g", S.google),
    ("h", S.hoogle),
    ("i", S.images),
    ("r", reddit),
    ("l", libgen),
    ("s", S.stackage),
    ("t", S.thesaurus),
    ("v", S.vocabulary),
    ("w", S.wikipedia),
    ("y", S.youtube)
  ]

myTabTheme = def {
    fontName            = myFont,
    activeColor         = bgColor0,
    inactiveColor       = bgColor0,
    activeBorderColor   = bgColor0,
    inactiveBorderColor = bgColor0,
    activeTextColor     = colorMagenta0,
    inactiveTextColor   = colorWhite3,
    decoHeight          = barHeight
  }

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
  map (first $ (,) controlMask) [
    (xK_z, killBefore),
    (xK_x, killAfter),
    (xK_Home, startOfLine),
    (xK_End, endOfLine),
    (xK_Left, moveCursor Prev),
    (xK_Right, moveCursor Next),
    (xK_BackSpace, killWord Prev),
    (xK_c, quit),
    (xK_v, pasteString) ] ++
    map (first $ (,) myModMask) [
    (xK_BackSpace, killWord Prev),
    (xK_f, moveWord Next),
    (xK_b, moveWord Prev),
    (xK_d, killWord Next),
    (xK_n, moveHistory W.focusUp'),
    (xK_p, moveHistory W.focusDown') ] ++
    map (first $ (,) 0) [
    (xK_Return, setSuccess True >> setDone True),
    (xK_KP_Enter, setSuccess True >> setDone True),
    (xK_BackSpace, deleteString Prev),
    (xK_Delete, deleteString Next),
    (xK_Left, moveCursor Prev),
    (xK_Right, moveCursor Next),
    (xK_Home, startOfLine),
    (xK_End, endOfLine),
    (xK_Down, moveHistory W.focusUp'),
    (xK_Up, moveHistory W.focusDown'),
    (xK_Escape, quit)
  ]

myXPConfig :: XPConfig
myXPConfig = def {
    font                = myFont,
    bgColor             = bgColor0,
    fgColor             = colorWhite0,
    bgHLight            = colorMagenta1,
    fgHLight            = colorBlack,
    borderColor         = colorBlue2,
    promptBorderWidth   = 0,
    promptKeymap        = myXPKeymap,
    position            = Top,
    height              = barHeight,
    historySize         = 256,
    historyFilter       = id,
    defaultText         = [],
    searchPredicate     = fuzzyMatch,
    sorter              = fuzzySort,
    autoComplete        = Nothing,
    showCompletionOnTab = False,
    defaultPrompter     = map toUpper,
    alwaysHighlight     = True,
    maxComplRows        = Just 5
  }

myPrograms :: [(String, String)]
myPrograms = [
    ("t", "telegram-desktop"),
    ("d", "discord"),
    ("s", "obs")
  ]

myScratchpads = [
    NS "Terminal" (myTerminal++" --title 'Terminal Scratchpad' -e zsh -c 'nvim-server; zsh'") (title =? "Terminal Scratchpad") (customFloating myFloatingRectangle),
    NS "Viewer" (myTerminal++" --title 'Viewer Scratchpad' -e zsh -c 'nvim-server; br'") (title =? "Viewer Scratchpad") (customFloating myFloatingRectangle),
    NS "Calculator" (myTerminal++" --title 'Calculator Scratchpad' -e qalc") (title =? "Calculator Scratchpad") (customFloating myFloatingRectangle),
    NS "System Monitor" (myTerminal ++ " --title 'System Scratchpad' -e btop") (title =? "System Scratchpad") (customFloating myFloatingRectangle),
    NS "Music Player" (myTerminal++" --title 'Music Player Scratchpad' -e mocp") (title =? "Music Player Scratchpad") (customFloating myFloatingRectangle)
  ]

nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

killAllFloating :: X ()
killAllFloating = ifWindows (className =? "Floating") (mapM_ killWindow) (return ())

myKeys :: [(String, X ())]
myKeys = [
    -- Prompts
    ("M-<Return>", do
        spawn "chlang us"
        shellPrompt myXPConfig
    ),
    ("M-M1-<Return>", do
        spawn "chlang us"
        manPrompt myXPConfig
    ),

    -- Kill stuff
    ("M-<Delete>", sequence_ [kill, BW.focusUp]),
    ("M-n", sequence_ [kill, BW.focusUp]),
    ("M-S-<Delete>", killAll),

    -- Quick Programs
    ("M-x", spawn (myTerminal ++ " --title 'Editor' -e zsh -c 'nvim-server; br; zsh'")),
    ("M-b", spawn myBrowser),
    ("M-a", spawn (myTerminal ++ " --title 'Terminal'")),

    -- Type email
    ("M-m", spawn "sh -c 'xsel -ib <<< \"r.a.maksimovich@gmail.com\"'"),

    -- Workspaces
    ("M-<Page_Down>", moveTo Next nonNSP),
    ("M-<Page_Up>", moveTo Prev nonNSP),
    ("M-M1-<Page_Down>", do
        shiftTo Next nonNSP
        moveTo Next nonNSP
    ),
    ("M-M1-<Page_Up>", do
        shiftTo Prev nonNSP
        moveTo Prev nonNSP
    ),
    ("M-M1-S-<Page_Down>", swapTo Next),
    ("M-M1-S-<Page_Up>", swapTo Prev),

    -- Scratchpads
    ("M-c", namedScratchpadAction myScratchpads "Terminal"),
    ("M-f", namedScratchpadAction myScratchpads "Viewer"),
    ("M-v", namedScratchpadAction myScratchpads "Music Player"),
    ("M-s", namedScratchpadAction myScratchpads "System Monitor"),
    ("M-q", namedScratchpadAction myScratchpads "Calculator"),

    -- Windows navigation
    ("M-<Up>", sendMessage $ Go U),
    ("M-<Down>", sendMessage $ Go D),
    ("M-<Left>", sendMessage $ Go L),
    ("M-<Right>", sendMessage $ Go R),
    ("M-M1-<End>", windows W.swapDown),
    ("M-M1-<Home>", windows W.swapUp),
    ("M-M1-<Up>", sendMessage $ Swap U),
    ("M-M1-<Down>", sendMessage $ Swap D),
    ("M-M1-<Left>", sendMessage $ Swap L),
    ("M-M1-<Right>", sendMessage $ Swap R),
    ("M-M1-C-<Up>", sendMessage $ Move U),
    ("M-M1-C-<Down>", sendMessage $ Move D),
    ("M-M1-C-<Left>", sendMessage $ Move L),
    ("M-M1-C-<Right>", sendMessage $ Move R),
    ("M-<Home>", BW.focusUp),
    ("M-<End>", BW.focusDown),
    ("M-d", do
        minimizedCount <- withMinimized $ return . length
        totalCount <- length . W.index . windowset <$> get
        if minimizedCount /= totalCount then withAll minimizeWindow else withAll maximizeWindow
    ),

    -- Layouts
    ("M-C-<Down>", sendMessage NextLayout),
    ("M-C-<Up>", sendMessage FirstLayout),
    ("M-C-<Left>", withFocused minimizeWindow),
    ("M-C-<Right>", withLastMinimized maximizeWindowAndFocus),
    ("M-C-p", spawn setWallpaperCmd),
    ("M-<Tab>", withFocused $ sendMessage . maximizeRestore),
    ("M-M1-<Tab>", sendMessage $ Toggle NBFULL),
    ("M-C-s", sendMessage $ Toggle MIRROR),
    ("M-C-x", sendMessage $ Toggle REFLECTX),
    ("M-C-v", sendMessage $ Toggle REFLECTY),
    ("M-C-f", sendMessage MG.Toggle),
    ("M-C-t", sinkAll),

    -- Window resizing
    ("M-C-,", sendMessage Shrink),
    ("M-C-.", sendMessage Expand),
    ("M-C-'", sendMessage MirrorShrink),
    ("M-C-;", sendMessage MirrorExpand),
    ("M-C-k", sequence_ [decScreenSpacing 1, decWindowSpacing 1]),
    ("M-C-l", sequence_ [incScreenSpacing 1, incWindowSpacing 1]),

    -- Keyboard Layouts
    ("M-e", spawn "chlang us"),
    ("M-r", spawn "chlang ru"),
    ("M-g", spawn "chlang de"),

    -- Multimedia Keys
    ("M-S-l", spawn "sleep 1 && xset dpms force off"),
    ("M-S-<Page_Down>", void (lowerVolume 5)),
    ("M-S-<Page_Up>", void (raiseVolume 5)),
    ("M-S-<End>", void toggleMute),
    ("M-S-,", spawn "brightnessctl set 1%-"),
    ("M-S-.", spawn "brightnessctl set 1%+"),
    ("M-S-<Home>", spawn "mocp --seek -50000"),
    ("M-S-<Down>", spawn "mocp --next") ,
    ("M-S-<Up>", spawn "mocp --previous"),
    ("M-S-<Right>", spawn "mocp --seek +5"),
    ("M-S-<Left>", spawn "mocp --seek -5"),
    ("M-<Space>", spawn "mocp --toggle-pause"),
    ("M-M1-<Space>", spawn "playerctl play-pause"),
    ("M-M1-S-<Down>", spawn "playerctl next"),
    ("M-M1-S-<Up>", spawn "playerctl previous"),
    ("M-p", spawn "flameshot gui --path $HOME/media/pictures"),
    ("M-S-p", spawn "flameshot full --path $HOME/media/pictures")
  ]
  ++ [("M-M1-" ++ show k, windows $ swapWithCurrent i) | (i,k) <- zip myWorkspaces [1..]]
  ++ [("M-/ " ++ k, S.promptSearch myXPConfig f) | (k,f) <- searchList ]
  ++ [("M-o " ++ k, spawn prog) | (k, prog) <- myPrograms]

-- Window rules:

myManageHook :: ManageHook
myManageHook = composeAll [
    insertPosition Below Newer,
    (title =? "Media viewer") <&&> (className =? "TelegramDesktop") --> doFloat
  ]

-- Xmobar settings:

myXmobarPP :: PP
myXmobarPP = def {
    ppSep               = ppSep,
    ppTitleSanitize     = xmobarStrip,
    ppCurrent           = magenta . wrap "[" "]",
    ppHidden            = blue,
    ppHiddenNoWindows   = lowWhite,
    ppUrgent            = red . wrap (yellow "!") (yellow "!"),
    ppOrder             = \ [ws, l, _, wins] ->
      let wws = words ws
          ws' = if length wws == length myWorkspaces then unwords wws else unwords . init $ wws
       in [ws', wins],
    ppExtras            = return $ concatLoggers [
        onLogger (\str -> if str == "0" then blue str else red str) minimizedLogger,
        onLogger (\str -> if str == "0" then blue str else yellow str) totalLogger,
        onLogger (white . last . words) logLayout
      ]
  }
  where
    totalLogger :: Logger
    totalLogger = do
        windows <- gets (W.index . windowset)
        return . Just . show $ length windows
    minimizedLogger :: Logger
    minimizedLogger = withMinimized $ return . return . show . length
    ppSep = " | "
    concatLoggers :: [Logger] -> Logger
    concatLoggers = fmap (fmap (intercalate ppSep) . sequence) . sequence
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor colorMagenta0 ""
    blue     = xmobarColor colorMagenta1 ""
    white    = xmobarColor colorWhite0 ""
    yellow   = xmobarColor colorYellow0 ""
    red      = xmobarColor colorRed0 ""
    lowWhite = xmobarColor colorWhite3 ""

-- myHandleEventHook = swallowEventHook (className =? "Alacritty" <&&> title =? "Viewer") (return True)
myHandleEventHook = mempty

main = xmonad . ewmhFullscreen . ewmh . withSB (statusBarProp "xmobar" (pure myXmobarPP)) $ docks defaults

defaults = def {
    terminal            = myTerminal,
    focusFollowsMouse   = myFocusFollowsMouse,
    focusedBorderColor  = colorMagenta0,
    normalBorderColor   = colorMagenta1,
    clickJustFocuses    = myClickJustFocuses,
    borderWidth         = windowBorderWidth,
    modMask             = myModMask,
    workspaces          = myWorkspaces,
    layoutHook          =
        minimize . BW.boringWindows
        $ maximizeWithPadding 0
        $ windowNavigation
        $ mkToggle (single NBFULL)
        $ avoidStruts
        $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) myLayout,
    manageHook          = myManageHook <+> namedScratchpadManageHook myScratchpads,
    handleEventHook     = myHandleEventHook,
    startupHook         = myStartupHook,
    logHook             = refocusLastLogHook >> nsHideOnFocusLoss myScratchpads
  } `additionalKeysP` myKeys

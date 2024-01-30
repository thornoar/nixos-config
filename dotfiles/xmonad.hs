-- Base
import XMonad
import qualified XMonad.StackSet as W
import System.Exit

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll, withAll)
import XMonad.Actions.Minimize
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo
import XMonad.Actions.GroupNavigation

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.List
import Data.Ratio
import Control.Monad (liftM2)

-- Hooks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.RefocusLast


-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import Data.Char (isSpace)
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch
import Control.Arrow (first)

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)
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

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.Simplest
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.NoBorders

ffmap :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
ffmap f ma mb = ma >>= (\x -> (fmap (f x)) mb)

myTerminal :: String
myTerminal = "$TERMINAL"

myBrowser :: String
myBrowser = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 0

myModMask :: KeyMask
myModMask = mod4Mask

myStartupHook :: X ()
myStartupHook = setWallpaperCmd

myWorkspaces :: [String]
myWorkspaces = ["fst", "snd", "aux"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myFont :: String
myFont = "xft:Hack Mono:mono:size=12:bold=false:antialias=true:hinting=true"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myFloatingRectangle :: W.RationalRect
myFloatingRectangle = W.RationalRect (1 % 6) (1 % 6) (2 % 3) (2 % 3)

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
    activeColor         = myBgColor,
    inactiveColor       = myBgColor,
    activeBorderColor   = myBgColor,
    inactiveBorderColor = myBgColor,
    activeTextColor     = colorMagenta0,
    inactiveTextColor   = colorLowWhite,
    decoHeight		    = myBarHeight
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
    (xK_v, pasteString) ] ++
    map (first $ (,) myModMask) [
    (xK_BackSpace, killWord Prev),
    (xK_Delete, quit),
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
    bgColor             = myBgColor,
    fgColor             = colorWhite,
    bgHLight            = colorBlue0,
    fgHLight            = colorBlack,
    borderColor         = colorBlue2,
    promptBorderWidth   = 0,
    promptKeymap        = myXPKeymap,
    position            = Top,
    height              = myBarHeight,
    historySize         = 256,
    historyFilter       = id,
    defaultText         = [],
    searchPredicate     = fuzzyMatch,
    sorter              = fuzzySort,
    autoComplete        = Nothing,
    showCompletionOnTab = False,
    defaultPrompter     = id $ map toUpper,
    alwaysHighlight     = True,
    maxComplRows        = Just 5 }

myPrograms :: [String]
myPrograms = [ myTerminal++" -e btop", "telegram-desktop", "discord", "obs", "goldendict" ]

myScratchpads = [
    NS "Scratchpad" (myTerminal++" --title 'Scratchpad'") (title =? "Scratchpad") (customFloating myFloatingRectangle),
    NS "Calculator" (myTerminal++" --title 'Calculator' -e qalc") (title =? "Calculator") (customFloating myFloatingRectangle),
    NS "GoldenDict" ("goldendict") (className =? "GoldenDict-ng") (customFloating myFloatingRectangle),
    NS "File Manager" (myTerminal ++ " --title 'File Scratchpad' -e zsh -c 'source $NIXOS_CONFIG/dotfiles/br.sh; $FILEMANAGER; zsh'") (title =? "File Scratchpad") (customFloating myFloatingRectangle),
    NS "Music Player" (myTerminal++" --title 'Music Player' -e mocp") (title =? "Music Player") (customFloating myFloatingRectangle)
    ]

nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

killAllFloating :: X ()
killAllFloating = ifWindows (className =? "Floating") (sequence_ . map killWindow) (return ())

myKeys :: [(String, X ())]
myKeys = [
    -- Xmonad
    -- ("M-M1-<Home>", do
    --     spawn (myTerminal ++ " --title 'Compiling' --class 'Floating' -e sh -c 'home-manager switch --impure --flake $NIXOS_CONFIG/; xmonad --recompile && xmonad --restart; exit'")
    -- ),
    -- ("M-M1-<End>", spawn "killall xmobar && xmobar --recompile &"),

    -- Prompts
    ("M-<Return>", shellPrompt myXPConfig),
    ("M-M1-<Return>", manPrompt myXPConfig),

    -- Kill windows
    ("M-<Delete>", kill),
    ("M-M1-<End>", killAllFloating),
    ("M-M1-<Delete>", killAll),

    -- Quick Programs
    ("M-x", spawn ( myTerminal ++ " --title 'File Manager' -e zsh -c 'source $NIXOS_CONFIG/dotfiles/br.sh; $FILEMANAGER; zsh'")),
    ("M-w", spawn myBrowser),
    ("M-a", spawn myTerminal),

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
    ("M-<Tab>", toggleWS' ["NSP"]),

    -- Scratchpads
    ("M-c", namedScratchpadAction myScratchpads "Scratchpad"),
    ("M-g", namedScratchpadAction myScratchpads "GoldenDict"),
    ("M-v", namedScratchpadAction myScratchpads "Music Player"),
    ("M-f", namedScratchpadAction myScratchpads "File Manager"),
    ("M-q", namedScratchpadAction myScratchpads "Calculator"),

    -- Windows navigation
    ("M-<Down>", sendMessage $ Go D),
    ("M-<Up>", sendMessage $ Go U),
    ("M-<Left>", sendMessage $ Go L),
    ("M-<Right>", sendMessage $ Go R),
    ("M-M1-<Left>", windows W.swapMaster),
    ("M-M1-<Down>", windows W.swapDown),
    ("M-M1-<Up>", windows W.swapUp),
    ("M-M1-<Right>", rotSlavesDown),
    ("M-<Home>", BW.focusUp),
    ("M-<End>", BW.focusDown),
    ("M-s", toggleFocus),
    -- ("M-d", withFocused minimizeWindow),
    ("M-d", do
        hasMinimized <- withMinimized $ return . (>0) . length
        if (hasMinimized) then withAll maximizeWindow else withAll minimizeWindow        
    ),

    -- Layouts
    ("M-C-<Down>", sendMessage NextLayout),
    ("M-C-<Up>", sendMessage FirstLayout),
    ("M-C-/", sendMessage (MT.Toggle NBFULL)),
    ("M-C-p", setWallpaperCmd),
    ("M-C-l", spawn "sleep 1 && xset dpms force off"),
    ("M-C-<Page_Up>", sendMessage (T.Toggle "simplestFloat")),
    ("M-C-<Page_Down>", withFocused $ windows . W.sink),
    ("M-C-t", sinkAll),

    -- Window resizing
    ("M-C-,", sendMessage Shrink),
    ("M-C-.", sendMessage Expand),
    ("M-C-'", sendMessage MirrorShrink),
    ("M-C-;", sendMessage MirrorExpand),
    ("M-C-j", sequence_ [decScreenSpacing 1, decWindowSpacing 1]),
    ("M-C-k", sequence_ [incScreenSpacing 1, incWindowSpacing 1]),

    -- Keyboard Layouts
    ("M-1", spawn "chlang us"),
    ("M-2", spawn "chlang ru"),
    ("M-3", spawn "chlang de"),

    -- Multimedia Keys
    ("M-S-<Page_Down>", spawn "amixer sset Master 5%-"),
    ("M-S-<Page_Up>", spawn "amixer sset Master 5%+"),
    ("M-S-,", spawn "brightnessctl set 1%-"),
    ("M-S-.", spawn "brightnessctl set 1%+"),
    ("M-S-<Home>", spawn "mocp --seek -500"),
    ("M-S-<Down>", spawn "mocp --next") ,
    ("M-S-<Up>", spawn "mocp --previous"),
    ("M-S-<Right>", spawn "mocp --seek +5"),
    ("M-S-<Left>", spawn "mocp --seek -5"),
    ("M-<Space>", spawn "mocp --toggle-pause"),
    ("M-z <Space>", spawn "playerctl play-pause"),
    ("M-z <Down>", spawn "playerctl next"),
    ("M-z <Up>", spawn "playerctl previous"),
    ("M-p", spawn "flameshot gui --path $HOME/media/pictures"),
    ("M-S-p", spawn "flameshot full --path $HOME/media/pictures")
    ]
    ++ [("M-/ " ++ k, S.promptSearch myXPConfig f) | (k,f) <- searchList ]
    ++ [("M-e " ++ (show k), spawn prog) | (k, prog) <- zip [1..(length myPrograms)] myPrograms]

-- Layouts:
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

named :: String -> l a -> ModifiedLayout Rename l a
named s = renamed [ Replace s ]

-- tall = 
--     named "Master & Slaves"
--     $ windowNavigation
--     -- $ smartBorders
--     $ avoidStruts
--     $ limitWindows 5
--     $ mySpacing' mySpace
--     $ ResizableTall 1 (3/100) (1/2) []

-- spirals =
--     named "Spirals"
--     $ windowNavigation
--     $ avoidStruts
--     $ mySpacing mySpace
--     $ spiral (6/7)

magnified =
    named "Magnified" 
    $ windowNavigation
    $ avoidStruts
    $ magnifier
    $ mySpacing mySpace
    $ Grid (16/10)

grid =
    named "Grid"
    $ windowNavigation
    $ avoidStruts
    $ subLayout [] (smartBorders Simplest)
    $ mySpacing mySpace
    $ mkToggle (single MIRROR)
    $ Grid (16/10)

tabs =
    named "Tabs"
    $ windowNavigation
    $ avoidStruts
    $ spacing (fromIntegral mySpace)
    $ tabbed shrinkText myTabTheme

myLayout = grid ||| Full ||| magnified ||| tabs

-- Window rules:

myIntersect :: [Bool] -> Bool
myIntersect = foldr (\a b -> a && b) True
myUnion :: [Bool] -> Bool
myUnion = foldr (\a b -> a || b) False

(/?) :: Eq a => Query a -> [a] -> Query Bool
q /? xs = (fmap myIntersect) $ sequence [ (fmap not) (q =? x) | x <- xs ]

(|?) :: Eq a => Query a -> [a] -> Query Bool
q |? xs = (fmap myUnion) $ sequence [ (q =? x) | x <- xs ]

-- viewShift = doF . liftM2 (.) W.greedyView W.shift

myAnd' :: Query Bool -> Bool -> Query Bool
myAnd' a b = (fmap (&& b)) a

myAnd :: Query Bool -> Query Bool -> Query Bool
myAnd a b = b >>= myAnd' a

myManageHook :: ManageHook
myManageHook = insertPosition Below Newer
-- myManageHook = composeAll [ insertPosition Below Newer ]
    -- (className /? [ "firefox", "Alacritty", "Zathura", "GoldenDict-ng", "Sxiv", "mpv", "Floating" ]) --> (viewShift ( last myWorkspaces )),
    -- title =? "Compiling" --> doRectFloat (W.RationalRect (3 % 5) (1 % 6) (7 % 20) (4 % 6)),
    -- (className |? [  ]) --> (viewShift ( last myWorkspaces )) ]

myXmobarPP :: PP
myXmobarPP = def {
    ppSep               = ppSep,
    ppTitleSanitize     = xmobarStrip,
    ppCurrent           = magenta . wrap "[" "]",
    ppHidden            = blue,
    ppHiddenNoWindows   = lowWhite,
    ppUrgent            = red . wrap (yellow "!") (yellow "!"),
    ppOrder             = \[ws, l, _, wins] -> [(unwords . (take 3) . words) ws, wins],
    ppExtras            = return $ concatLoggers    [
        onLogger (\str -> if (str == "0") then (blue str) else (red str)) minimizedLogger,
        onLogger (\str -> if (str == "0") then (blue str) else (yellow str)) maximizedLogger,
        onLogger (white . drop 9) logLayout
        ]
    } where
    totalLogger :: Logger
    totalLogger = do
        windows <- gets (W.index . windowset)
        return . Just . show $ (length windows)

    minimizedLogger :: Logger
    minimizedLogger = withMinimized $ return . return . show . length

    strsb :: String -> String -> String
    strsb s1 s2 = show $ (read s1) - (read s2)
    subtract :: Logger -> Logger -> Logger
    subtract = ffmap (ffmap strsb)
    maximizedLogger :: Logger
    maximizedLogger = subtract totalLogger minimizedLogger

    ppSep = " | "
    concatLoggers :: [Logger] -> Logger
    concatLoggers = (fmap (fmap $ intercalate ppSep)) . (fmap sequence) . sequence

    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor colorMagenta0 ""
    blue     = xmobarColor colorBlue0 ""
    white    = xmobarColor colorWhite ""
    yellow   = xmobarColor colorYellow ""
    red      = xmobarColor colorRed ""
    lowWhite = xmobarColor colorLowWhite""

-- myHandleEventHook = swallowEventHook (title =? "File Manager") (className |? ["Zathura", "mpv"])
-- myHandleEventHook = swallowEventHook (return True) (return True)
myHandleEventHook = mempty

main =
    xmonad $
    ewmhFullscreen .
    ewmh .
    withSB (statusBarProp "xmobar" (pure myXmobarPP)) $ docks $
    defaults

defaults = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    focusedBorderColor = colorMagenta0,
    normalBorderColor  = colorBlue0,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    layoutHook         = minimize . BW.boringWindows $ myLayout,
    manageHook         = myManageHook <+> namedScratchpadManageHook myScratchpads,
    handleEventHook    = myHandleEventHook,
    startupHook        = myStartupHook,
    logHook            = refocusLastLogHook }
    `additionalKeysP` myKeys

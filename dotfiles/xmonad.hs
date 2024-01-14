-- Base
import XMonad
import qualified XMonad.StackSet as W
import System.Exit
import Data.List

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.Minimize
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
-- import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.InsertPosition

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
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
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedWindows
import XMonad.Util.Loggers

-- Xmobar
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.ManageHelpers

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
myStartupHook = setWallpaperCmd--spawnOnce "feh --randomize --bg-fill $WALLPAPERS"

myWorkspaces :: [String]
myWorkspaces = [" 1 ", " 2 ", " 3 "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

myFont :: String
myFont = "xft:Hack Mono:mono:size=12:bold=false:antialias=true:hinting=true"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

archwiki, nixoswiki, reddit, libgen :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
nixoswiki     = S.searchEngine "nixoswiki" "https://nixos.wiki/index.php?search="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
libgen   = S.searchEngine "libgen" "https://www.libgen.is/search.php?req="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("n", nixoswiki)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("r", reddit)
             , ("l", libgen)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             ]

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#89a870"  
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#89a870"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#03051e"
                 , inactiveTextColor   = "#a2afbe"
                 , decoHeight		   = myBarHeight
                 }

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)      -- control + <key>
     [ (xK_z, killBefore)               -- kill line backwards
     , (xK_x, killAfter)                -- kill line forwards
     , (xK_Home, startOfLine)              -- move to the beginning of the line
     , (xK_End, endOfLine)                -- move to the end of the line
     , (xK_Left, moveCursor Prev)          -- move cursor forward
     , (xK_Right, moveCursor Next)          -- move cursor backward
     , (xK_BackSpace, killWord Prev)    -- kill the previous word
     , (xK_v, pasteString)              -- paste a string
     ]
     ++
     map (first $ (,) myModMask)          -- meta key + <key>
     [ (xK_BackSpace, killWord Prev)    -- kill the prev word
     , (xK_c, quit)                     -- quit out of prompt
     , (xK_f, moveWord Next)            -- move a word forward
     , (xK_b, moveWord Prev)            -- move a word backward
     , (xK_d, killWord Next)            -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = myBgColor--"#282c34"
      , fgColor             = colorWhite--"#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      , position            = Top
      , height              = myBarHeight
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      , alwaysHighlight     = True
      , maxComplRows        = Just 5      -- set to 'Just 5' for 5 rows
      }

myPrograms :: [String]
myPrograms = [ myTerminal++" -e btop", "telegram-desktop", "discord", "obs", "goldendict" ]

myKeys :: [(String, X ())]
myKeys = [
    -- Xmonad
         ("M-M1-<Home>", spawn (myTerminal ++ " --hold -e sh -c 'home-manager switch --impure --flake $NIXOS_CONFIG/; xmonad --recompile; xmonad --restart; echo Done!'")) -- Recompiles xmonad

    -- Run Prompt
        , ("M-<Return>", shellPrompt myXPConfig) -- Xmonad Shell Prompt

    -- Other Prompts
        , ("M-<Tab> m", manPrompt myXPConfig)          -- manPrompt
        , ("M-<Tab> x", xmonadPrompt myXPConfig)       -- xmonadPrompt

    -- Kill windows
        , ("M-c", kill)     -- Kill the currently focused client
        , ("M-M1-a", killAll)   -- Kill all windows on current workspace

	-- Quick Programs
		, ("M-e", spawn ( myTerminal ++ " -e $FILEMANAGER" ))
		, ("M-x", spawn ( myTerminal ++ " -e zsh -c 'cd $PROJECTS && nvim'" ))
		, ("M-v", spawn ( myTerminal ++ " -e mocp" ))
		, ("M-w", spawn myBrowser)
		, ("M-a", spawn myTerminal)
	
    -- Workspaces
	    , ("M-<Page_Down>", nextWS)
		, ("M-<Page_Up>", prevWS)
		, ("M-M1-<Page_Down>", do
			shiftToNext
  			nextWS
  		)
		, ("M-M1-<Page_Up>", do
			shiftToPrev
			prevWS
  		)

    -- Windows navigation
		, ("M-<Down>", sendMessage $ Go D)
		, ("M-<Up>", sendMessage $ Go U)
		, ("M-<Left>", sendMessage $ Go L)
		, ("M-<Right>", sendMessage $ Go R)
		, ("M-/", windows W.focusDown)
		, ("M-M1-<Left>", windows W.swapMaster)
		, ("M-M1-<Down>", windows W.swapDown)
		, ("M-M1-<Up>", windows W.swapUp)
        , ("M-M1-<Right>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
		, ("M-<Home>", windows W.focusUp)
		, ("M-<End>", windows W.focusDown)
		, ("M-d", withFocused minimizeWindow)
		, ("M-b", withLastMinimized maximizeWindowAndFocus)

    -- Layouts
        , ("M-C-<Down>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-<Up>", sendMessage FirstLayout)           -- Switch to next layout
		, ("M-C-/", sendMessage (MT.Toggle NBFULL)) -- Toggles noborder
	    , ("M-C-p", setWallpaperCmd)
        , ("M-C-l", spawn "sleep 1 && xset dpms force off")
        , ("M-C-<Page_Up>", sendMessage (T.Toggle "simplestFloat")) -- Toggles my 'floats' layout
        , ("M-C-<Page_Down>", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-C-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Window resizing
        , ("M-C-,", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-C-.", sendMessage Expand)                   -- Expand horiz window width
        , ("M-C-'", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-C-;", sendMessage MirrorExpand)          -- Expand vert window width
        , ("M-C-j", sequence_ [decScreenSpacing 1, decWindowSpacing 1]) 
        , ("M-C-k", sequence_ [incScreenSpacing 1, incWindowSpacing 1]) 

	-- Keyboard Layouts
		, ("M-1", spawn "chlang us")
		, ("M-2", spawn "chlang ru")
		, ("M-3", spawn "chlang de")

    -- Multimedia Keys
		, ("M-S-<Page_Down>", spawn "amixer sset Master 5%-")
		, ("M-S-<Page_Up>", spawn "amixer sset Master 5%+")
        , ("M-S-,", spawn "brightnessctl set 1%-")
        , ("M-S-.", spawn "brightnessctl set 1%+")
		, ("M-S-<Home>", spawn "mocp --seek -500")
		, ("M-S-<Down>", spawn "mocp --next") 
		, ("M-S-<Up>", spawn "mocp --previous")
		, ("M-S-<Right>", spawn "mocp --seek +5")
		, ("M-S-<Left>", spawn "mocp --seek -5")
		, ("M-<Space>", spawn "mocp --toggle-pause")
        , ("M-z <Space>", spawn "playerctl play-pause")
        , ("M-z <Right>", spawn "playerctl next")
        , ("M-z <Left>", spawn "playerctl previous")
		, ("M-s", spawn "flameshot gui --path $HOME/media/Pictures")
        , ("M-S-s", spawn "flameshot full --path $HOME/media/Pictures")
        ]
        ++ [("M-f " ++ k, S.promptSearch myXPConfig f) | (k,f) <- searchList ]
        ++ [("M-S-f " ++ k, S.selectSearch f) | (k,f) <- searchList ]
		++ [("M-q " ++ (show k), spawn prog) | (k, prog) <- zip [1..(length myPrograms)] myPrograms]

-- Layouts:
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall = renamed [ Replace "Master & Slaves" ]
           $ windowNavigation
           $ avoidStruts
           $ limitWindows 5
           $ mySpacing' mySpace
           $ ResizableTall 1 (3/100) (1/2) []
magnified = renamed [ Replace "Magnified" ]
           $ windowNavigation
           $ avoidStruts
           $ magnifier
		   $ mySpacing' mySpace
           $ limitWindows 12
           $ ResizableTall 1 (3/100) (1/2) []
grid = renamed [ Replace "Grid" ]
           $ windowNavigation
           $ avoidStruts
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing' mySpace
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals = renamed [ Replace "Spirals" ]
           $ mySpacing mySpace
           $ avoidStruts
		   $ windowNavigation
		   $ spiral (6/7)
threeCol = renamed [ Replace "ThreeColumns" ]
           $ windowNavigation
           $ avoidStruts
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
tabs = renamed [ Replace "Tabs" ]
           $ windowNavigation
           $ avoidStruts
           $ mySpacing' mySpace
           $ tabbed shrinkText myTabTheme

-- Window rules:
myManageHook = insertPosition Below Newer
myEventHook = mempty
myLogHook = return ()

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = ppSep
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = magenta . cutwrap "[" "]"-- . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = blue-- . wrap "" ""
    , ppHiddenNoWindows = lowWhite-- . wrap "" ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, wins]
    , ppExtras          = return $ concatLoggers [
                            onLogger (\str -> if (str == "0") then (blue str) else (red str)) minimizedLogger,
                            onLogger (\str -> if (str == "0") then (blue str) else (yellow str)) logF,
                            onLogger (white . drop 9) logLayout
                          ]
    }
    where
        logF :: Logger
        logF = do
            windows <- gets (W.index . windowset)
            return . Just . show $ (length windows)

        minimizedLogger :: Logger
        minimizedLogger = withMinimized $ return . return . show . length
        -- minimizedLogger = withMinimized $ (\wins -> return (Just ((show . length) wins)))

        ppSep = " | "
        concatLoggers :: [Logger] -> Logger
        concatLoggers = (fmap (fmap $ intercalate ppSep)) . (fmap sequence) . sequence

        -- cutwrap str1 str2 xs = wrap str1 str2 (tail (init xs))
        cutwrap d1 d2 = (wrap d1 d2) . trim

        -- myPPLayout = (++" ") . (" "++) . drop 9
        myPPLayout = drop 9

        formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, magenta, red, white, yellow :: String -> String
        magenta  = xmobarColor colorMagenta ""
        blue     = xmobarColor colorBlue ""
        white    = xmobarColor colorWhite ""
        yellow   = xmobarColor "#f1fa8c" ""
        red      = xmobarColor "#ff5555" ""
        lowWhite = xmobarColor "#bbbbbb" ""


main = xmonad $
       ewmhFullscreen .
       ewmh .
       -- withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey $
       withSB (statusBarProp "xmobar" (pure myXmobarPP)) $ docks $
       defaults

defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        layoutHook         = minimize . BW.boringWindows $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
		logHook            = myLogHook
    } `additionalKeysP` myKeys

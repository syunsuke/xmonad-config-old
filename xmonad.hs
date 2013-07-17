{-
Form Xmonad/Config archive
http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
/Brent Yorgey's darcs xmonad.hs (darcs)

my custom xmonad config file

-}

import XMonad

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List ((\\), find)
import Data.Maybe (isJust, catMaybes)
import Data.Monoid
import System.Posix.Unistd
--import Control.Concurrent (threadDelay)
--import Control.Monad (when)
import XMonad.ManageHook

-- Hooks -----------------------------------------------------
import XMonad.Hooks.DynamicLog
  hiding (pprWindowSet)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

-- Layout ----------------------------------------------------
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed

import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.MagicFocus
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- Actions ---------------------------------------------------
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.WithAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
--import XMonad.Actions.FloatKeys

-- Prompts ---------------------------------------------------
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------
--import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

-- end of module----------------------------------------------


data Host = Desktop | Laptop
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "prime"      -> Desktop
    "madokita"   -> Laptop
    _            -> Desktop

myNaviColor     = "#772211"
myFoucusedColor = "#ffa000"
myNormalColor   = "#222222"

main :: IO ()
main = do

  my_host    <- getHost
  status_bar <- spawnPipe $ myDzenStatus my_host
  spawn $ myConkyStatus my_host

  xmonad $ myxmonadConfig status_bar my_host

  where
    -- ホスト毎のステータスバー設定
    ----------------------------------------------------------------------------

    -- 分割部分の位置
    myStatusLength host =
      case host of Desktop -> 900
                   _       -> 400


    myStatusStartPoint host =
      case host of Desktop -> 1280
                   _       -> 0
   

    -- ステータスバーの高さとフォント
    myDzenStyle host  =
      case host of Desktop ->  " -h '20' -fg '#aaaaaa' -bg '#000000' -fn 'M+ 1mn:size=11'"
                   _       ->  " -h '18' -fg '#aaaaaa' -bg '#000000' -fn 'M+ 1mn:size=10'"

    -- ホストでConkyの内容を変える
    myconkyrc host =
      case host of Desktop ->  "conky_dzen"
                   _       ->  "conky_dzen_laptop"

    -- 以下共通
    myDzenStatus host = "dzen2 -x " ++ show (myStatusStartPoint host)
                        ++ " -w " ++ show (myStatusLength host)
                        ++ " -ta 'l'"
                        ++ (myDzenStyle host)

    myConkyStatus host = "conky -c ~/.xmonad/"
                         ++ (myconkyrc host)
                         ++ " | dzen2  -x "
                         ++ show ((myStatusStartPoint host) + (myStatusLength host))
                         ++" -ta r " ++ (myDzenStyle host)

-- Bascic Config ------------------------------------------------------

myTerminal = "urxvt"
myShell    = "zsh"

myxmonadConfig h host =
  defaultConfig
    { terminal           = myTerminal

    , modMask            = mod4Mask
    , focusFollowsMouse  = True
    , borderWidth        = 2
    , normalBorderColor  = myNormalColor
    , focusedBorderColor = myFoucusedColor

    , workspaces         = myTopicNames host

    , logHook            = myLogHook h host

    , layoutHook         = myLayoutHook host
    , manageHook         = myManageHook host

                           -- 特定の場合のみマウスカーソルでフォーカス移動
    , handleEventHook    = followOnlyIf (queryFocused whenToFollow)
                           <+> myEventHook

                           -- keyマップの整合性をチェックしてくれる
    , startupHook        = return () >>
                           checkKeymap (myxmonadConfig h host)
                                       (myKeys h host)
    }
    `additionalKeysP` (myKeys h host)


-- Workspace(Topicspace) -------------------------------------------------------
-- From Brent Yorgey's darcs xmonad.hs

data TopicItem = TI { topicName :: Topic
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }


myTopics :: Host -> [TopicItem]
myTopics host =
  [ti "home"         ""
  , TI "web"         ""         (spawn "chromium")
  , TI "navi2ch"     ""         (spawn "emacs -f navi2ch"
                                 >> spawn "firefox")
  , TI "v2c"         ""         (spawn "local/v2c/v2c")
  , TI "xm-conf"     ".xmonad"  (edit "~/.xmonad/xmonad.hs"
                                 >> spawn "firefox --new-window http://www.xmonad.org")
  , TI "irc"         ""         (spawn "xchat")
  , ti "NSP"         ""
  ]
  where
    ti t d = TI t d shell
    shell = spawnShell host

spawnShell :: Host -> X ()
spawnShell host = currentTopicDir (myTopicConfig host) >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal
                   ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"

myTopicNames :: Host -> [Topic]
myTopicNames = map topicName . myTopics

myTopicConfig :: Host -> TopicConfig
myTopicConfig host = defaultTopicConfig
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics'
  , defaultTopicAction = const (return ())
  , defaultTopic = "home"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics'
  }
  where myTopics' = myTopics host


edit :: String -> X ()
edit = spawn . ("emacs --no-splash "++)


goto :: Host -> Topic -> X ()
goto host t = switchTopic (myTopicConfig host) t

promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto

promptedGotoOtherScreen :: Host -> X ()
promptedGotoOtherScreen host =
  workspacePrompt myXPConfig $ \ws -> do            -- (27)
    nextScreen
    goto host ws



promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- xmonad自前のプロンプト表示用のリソース
myXPConfig = defaultXPConfig
    { fgColor = "orange"
    , bgColor = "black"
    , font    = "xft:Ricty:pixelsize=20:autohint=true"
    , position = Bottom
    , height   = 30
    }



--- manage -----------------------------------------------------------
myManageHook host = manageDocks <+>
               (namedScratchpadManageHook (scratchpads host) ) <+>
               (composeAll (
               [ isFullscreen              --> doFullFloat ]

               ++
               [ className =? c --> doCenterFloat | c <- myCenterFloats ]

               ++
               [ className =? c --> doFloat       | c <- myFloats  ]))

               where
                 myCenterFloats = [ "feh"
                                  , "Xmessage"
                                  , "gnome-search-tool"
                                  ]
                 myFloats       = [ "xev"
                                  , "MPlayer"
                                  , "Gimp"
                                  ]

--- scratchpad --------------------------------------------------------
scratchpadSize host = case host of
  Desktop ->   W.RationalRect (1/4) (1/4) (1/2) (1/2)
  _       ->   W.RationalRect (1/6) (1/6) (4/6) (4/6)

mySPFloat host = customFloating $ scratchpadSize host

customTerm = "urxvt -depth 32 -bg '[90]#003f3f' "
--customTerm = "urxvt "

scratchpads host =
  [ NS "s_term1" (customTerm ++ "-title s_term1") (title =? "s_term1") (mySPFloat host)
  , NS "s_term2" (customTerm ++ "-title s_term2") (title =? "s_term2") defaultFloating
  ]



--- Log to statusbar ---------------------------------------------------

myLogHook h host = dynamicLogWithPP $ defaultPP {
  ppCurrent = dzenColor "#00ffaa" "" . wrap "[" "]",
  ppHidden  = dzenColor "#00aa11" "" . wrap "" "",
  ppUrgent  = dzenColor "#ff0000" "" . wrap " " " ",
  ppSep     = " : ",

  ppLayout  = dzenColor "#aaaaaa" "" .
              wrap "^ca(1,xdotool key super+space) " " ^ca()",
  ppTitle   = dzenColor "#ffcc55" "#555555"
              . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c) "
                     "                    ^ca()^ca()" . (myshorten host) . dzenEscape,
--  ppSort    = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder,
  ppOutput  = hPutStrLn h

  }
  where
    myshorten host =
      case host of Desktop ->  shorten 50
                   _       ->  shorten 30



--- my Layout customize --------------------------------------------------

myLayoutHook host =

  avoidStruts $

  gaps (zip [U,D,L,R] (repeat 3)) $

  configurableNavigation (navigateColor myNaviColor) $

  mkToggle1 NBFULL $
  mkToggle1 REFLECTX $
  mkToggle1 REFLECTY $
  mkToggle1 NOBORDERS $
  mkToggle1 MIRROR $

  smartBorders $

  onWorkspaces ["xm-conf"] (myEditorLayout ||| myTiled) $
  onWorkspace "navi2ch" (myNavi2Layout ||| Full) $

  myTiled |||
  Mag.magnifier Grid |||
  (TwoPane (3/100) (1/2)) |||
  (renamed[Replace "Full|Acc"] $ combineTwo myTiled Full Accordion)

myTiled = renamed[Replace "Tall"]  $ spacing 3 $ Tall 1 0.03 0.6

myEditorLayout = renamed[Replace "emacs|tab"] $
               combineTwoP
               (spacing 3 $ TwoPane 0.03 0.53)
               (simpleTabbed)
               (simpleTabbed)
               (ClassName  "Emacs")

myNavi2Layout = renamed[Replace "navi2ch|mag"] $
                Mag.magnifier $ 
                combineTwoP
               (spacing 3 $ TwoPane 0.03 (3/4))
               (simpleTabbed)
               (Mag.magnifier Grid)
               (ClassName  "Emacs")


wwwcomboL  = renamed[Replace "www_L"] $
             combineTwoP
             ( spacing 3 $ TwoPane 0.03 0.60)
             (simpleTabbed)
             (simpleTabbed)
             (ClassName  "Chromium" `Or` ClassName  "Firefox")

wwwcomboU  = renamed[Replace "www_U"] $
             combineTwoP
             ( Mirror $ spacing 3 $ TwoPane 0.03 0.70)
             (simpleTabbed) (simpleTabbed)
             (ClassName  "Chromium" `Or` ClassName  "Firefox")



-- misc -----------------------------------------------------------------------
myEventHook = docksEventHook <+> fullscreenEventHook   ---(100)
--myEventHook = docksEventHook



-- Customize Keys --------------------------------------------------------------
myKeys h host = myKeymap host (myxmonadConfig h host)
myKeymap host conf =

  -- mod-[1..],       Switch to workspace N
  -- mod-shift-[1..], Move client to workspace N
  [(m ++ "M-" ++ [k], f i)
   | (i,k) <- zip (XMonad.workspaces conf) "1234567890"
   , (f,m) <- [(goto',"")
              ,(windows . W.shift, "S-")
              ,(\ws -> nextScreen >> (goto' $ ws), "C-")
              ]
  ]
  ++

  -- 制御系
  [
    -- workspace,topicspase移動
    ("M-g",   promptedGoto host)
  , ("M-C-g", promptedGotoOtherScreen host)
  , ("M-S-g", promptedShift)
  , ("M-S-C-g", workspacePrompt myXPConfig $
                \ws -> withAll' (W.shiftWin ws) >> goto host ws)

  , ("M-w", screenWorkspace 1 >>= flip whenJust (windows . W.view))
  , ("M-e", screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ("M-s", nextScreen)

  , ("M-b",  toggleWS' ["NSP"])
  , ("M-m",  DO.moveTo Next HiddenNonEmptyWS)
  , ("M-n",  DO.moveTo Prev HiddenNonEmptyWS)
  , ("M-C-<R>",  DO.moveTo Next HiddenNonEmptyWS)
  , ("M-C-<L>",  DO.moveTo Prev HiddenNonEmptyWS)

    -- window navigation keybindings.
  , ("M-<R>" , sendMessage $ Go R)
  , ("M-<L>" , sendMessage $ Go L)
  , ("M-<U>" , sendMessage $ Go U)
  , ("M-<D>" , sendMessage $ Go D)

  , ("M-S-<R>", sendMessage $ Swap R)
  , ("M-S-<L>", sendMessage $ Swap L)
  , ("M-S-<U>", sendMessage $ Swap U)
  , ("M-S-<D>", sendMessage $ Swap D)

  , ("M-S-C-<R>", sendMessage $ Move R)
  , ("M-S-C-<L>", sendMessage $ Move L)
  , ("M-S-C-<U>", sendMessage $ Move U)
  , ("M-S-C-<D>", sendMessage $ Move D)

  , ("M-S-<Return>", windows W.swapMaster)

    --テンキー用
  , ("<KP_Right>", sendMessage $ Go R)
  , ("<KP_Left>" , sendMessage $ Go L)
  , ("<KP_Up>"   , sendMessage $ Go U)
  , ("<KP_Down>" , sendMessage $ Go D)

  , ("S-<KP_Right>", sendMessage $ Swap R)
  , ("S-<KP_Left>" , sendMessage $ Swap L)
  , ("S-<KP_Up>"   , sendMessage $ Swap U)
  , ("S-<KP_Down>" , sendMessage $ Swap D)

  , ("S-C-<KP_Right>", sendMessage $ Move R)
  , ("S-C-<KP_Left>" , sendMessage $ Move L)
  , ("S-C-<KP_Up>"   , sendMessage $ Move U)
  , ("S-C-<KP_Down>" , sendMessage $ Move D)

  , ("<KP_Page_Down>",  DO.moveTo Next HiddenNonEmptyWS)
  , ("<KP_End>"      ,  DO.moveTo Prev HiddenNonEmptyWS)

  , ("<KP_Page_Up>"   ,  swapNextScreen)
  , ("<KP_Home>"      ,  shiftNextScreen)

  , ("<KP_Begin>"      , windows W.focusMaster)

  , ("M-<KP_Enter>"    , windows W.swapMaster)


    -- expand/shrink windows
  , ("M-h",   sendMessage Shrink)
  , ("M-l",   sendMessage Expand)

    -- toggles: fullscreen, flip x, flip y, mirror, no borders, outer gap
  , ("M-f",   sendMessage $ Toggle NBFULL)
  , ("M-C-x", sendMessage $ Toggle REFLECTX)
  , ("M-C-y", sendMessage $ Toggle REFLECTY)
  , ("M-C-m", sendMessage $ Toggle MIRROR)
  , ("M-C-b", sendMessage $ Toggle NOBORDERS)
--  , ("M-C-g", sendMessage $ ToggleGaps)

  , ("M-<Print>",       sendMessage $ Toggle REFLECTX)  --another key
  , ("M-<Scroll_lock>", sendMessage $ Toggle REFLECTY)  --another key
  , ("M-<Pause>",       sendMessage $ Toggle MIRROR)    --another key

  ]
  ++

  -- 呼び出し系
  [
    -- scratch pad
    ("M-o", namedScratchpadAction (scratchpads host) "s_term1")
  , ("M-p", namedScratchpadAction (scratchpads host) "s_term2")

    -- ターミナル起動
  , ("M-<Return>"  , spawnShell host)

    
    -- プログラムランチャー
  , ("M-r", spawn ("dmenu_run -b -fn '" ++ "Ricty:size=15" ++ "'"))

  , ("M-x f", spawn "firefox")
  , ("M-x c", spawn "chromium")
  , ("M-x x", spawnShell host)
  ]
  ++

  -- 終了用とか
  [ ("M-S-c", kill)
  , ("M-S-C-c", killAll)
  ]

  where goto' = goto host


-- (29) Focus follows mouse only for Gimp windows
-- 一致する場合に追いかけると言うより
-- 一致しないもののウインドに入ると追いかけるのをやめる
whenToFollow :: Query Bool
whenToFollow = (className =? "Gimp")
--               <||> (className =? "Chromium")
--               <||> (className =? "Emacs")

queryFocused :: Query Bool -> X Bool
queryFocused q = withWindowSet $ maybe (return False) (runQuery q) . W.peek


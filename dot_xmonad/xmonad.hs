import Data.Tree
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.TreeSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Circle (Circle (..))
import XMonad.Layout.Column
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes

myWorkspaces :: Forest String
myWorkspaces =
  [ Node
      "home"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "browse"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "chat"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "code"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "game"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "work"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ],
    Node
      "music"
      [ Node "alpha" [],
        Node "beta" [],
        Node "gamma" []
      ]
  ]

myTreeConf =
  tsDefaultConfig
    { ts_font = "xft:Terminess Powerline",
      ts_background = 0xc0c5d0da,
      ts_node = (0xff0d0931, 0xff3cbff0),
      ts_nodealt = (0xffffffff, 0xff0d0931),
      ts_highlight = (0xffffffff, 0xffE72478),
      ts_extra = 0xff000000
    }

myLayouts =
  avoidStruts
    $ spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
    $ emptyBSP
      ||| tabbed shrinkText (theme smallClean)
      ||| Column (10 / 7)
      ||| Full

main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar -o"
  spawn "nitrogen --restore"
  spawn "compton"
  spawn "bash ~/.screenlayout/default.sh"
  xmonad $
    ewmh
      defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig,
          workspaces = toWorkspaces myWorkspaces,
          logHook = dynamicLogWithPP $ sjanssenPP {ppOutput = hPutStrLn xmobar},
          layoutHook = myLayouts,
          handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
          borderWidth = 1,
          terminal = "urxvt",
          normalBorderColor = "#053569",
          focusedBorderColor = "#0954B5",
          focusFollowsMouse = False
        }
      `additionalKeys` [ ((mod1Mask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.greedyView),
                         ((mod1Mask .|. shiftMask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.shift),
                         ((mod1Mask, xK_p), spawn "rofi -show run -theme Paper"),
                         ((mod1Mask .|. shiftMask, xK_p), spawn "rofi -show window -theme Paper"),
                         ((mod1Mask .|. shiftMask, xK_t), sendMessage ToggleStruts)
                       ]

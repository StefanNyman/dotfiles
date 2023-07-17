import           Data.Char
import qualified Data.Map                            as M
import           Keyboard
import           System.Exit
import           System.IO
import           XMonad                              hiding (setLayout)
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.BinarySpacePartition  as BSP
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Run

myTerminal = "wezterm"
myModMask = mod4Mask
altMask = mod1Mask
ctrlMask = controlMask
myBorderWidth = 0
myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

myKeys conf@XConfig {XMonad.modMask = modm} =
  let m = modMask conf
   in M.fromList $
        [ -- launch terminal
          ((modm, xK_Return), spawn $ XMonad.terminal conf),
          -- launch dmenu
          ((modm, xK_f), spawn "exe=`dmenu_path| dmenu` && eval \"exec $exe\""),
          -- lock screen
          ((modm .|. shiftMask, xK_p), spawn "xsecurelock"),
          -- kill focused window
          ((modm .|. shiftMask, xK_c), kill),
          -- quit xmonad
          ((modm .|. shiftMask, xK_q), io exitSuccess),
          -- restart xmonad
          ((modm, xK_q), restart "xmonad" True),
          -- re-tile window
          ((modm, xK_t), withFocused $ windows . W.sink),
          -- swap keyboard layout between us and se
          ((modm .|. ctrlMask, xK_space), swapLayout [US, SE]),
          -- toggle fullscreen
          ((modm, xK_p), sendMessage $ Toggle FULL),
          -- BSP bindings
          --
          -- expand window right
          ((modm .|. altMask, xK_n), sendMessage $ BSP.ExpandTowards BSP.R),
          -- expand window left
          ((modm .|. altMask, xK_h), sendMessage $ BSP.ExpandTowards BSP.L),
          -- expand window down
          ((modm .|. altMask, xK_t), sendMessage $ BSP.ExpandTowards BSP.D),
          -- expand window up
          ((modm .|. altMask, xK_s), sendMessage $ BSP.ExpandTowards BSP.U),
          -- shrink window from right
          ((modm .|. altMask .|. shiftMask, xK_n), sendMessage $ BSP.ShrinkFrom BSP.R),
          -- shrink window from left
          ((modm .|. altMask .|. shiftMask, xK_h), sendMessage $ BSP.ShrinkFrom BSP.L),
          -- shrink window from down
          ((modm .|. altMask .|. shiftMask, xK_t), sendMessage $ BSP.ShrinkFrom BSP.D),
          -- shrink window from up
          ((modm .|. altMask .|. shiftMask, xK_s), sendMessage $ BSP.ShrinkFrom BSP.U),
          -- rotate layout
          ((modm, xK_l), sendMessage BSP.Rotate),
          -- swap
          ((modm, xK_d), sendMessage BSP.Swap),
          -- focus parent
          ((modm, xK_w), sendMessage BSP.FocusParent),
          -- select node
          ((modm .|. ctrlMask, xK_h), sendMessage BSP.SelectNode),
          -- move node
          ((modm .|. shiftMask, xK_t), sendMessage BSP.MoveNode),
          -- split shift prev
          ((modm .|. shiftMask .|. ctrlMask, xK_s), sendMessage $ BSP.SplitShift Prev),
          -- split shift next
          ((modm .|. shiftMask .|. ctrlMask, xK_n), sendMessage $ BSP.SplitShift Next),
          -- balance layout
          ((modm, xK_a), sendMessage BSP.Balance),
          -- equalize, gives all windows the same size
          ((modm .|. shiftMask, xK_a), sendMessage BSP.Equalize),
          -- go to right window
          ((modm, xK_n), windowGo R False),
          -- go to left window
          ((modm, xK_h), windowGo L False),
          -- go to up window
          ((modm, xK_s), windowGo U False),
          -- go to down window
          ((modm, xK_t), windowGo D False),
          -- toggle struts
          ((modm, xK_b), sendMessage ToggleStruts)
        ]
          ++
          -- mod-[0..9], switch to workspace n
          -- mod-shift-[0..9], move window to workspace n
          [ ((m .|. modm, k), windows $ onCurrentScreen f i)
            | (i, k) <- zip (workspaces' conf) [xK_0 .. xK_9],
              (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
          ]
          ++
          -- mod-{l,d} switch to physical screen
          -- mod-shift-{l,d}, move window to physical screen
          [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_l, xK_d] [0 ..],
              (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
          ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ -- mod-button1 set window to floating and move by dragging
      ((modm, button1), \w -> focus w >> mouseMoveWindow w),
      -- mod-button2 raise the window to top of stack
      ((modm, button2), \w -> focus w >> windows W.swapMaster),
      -- mod-button3, set window to floating and resize by dragging
      ((modm, button3), \w -> focus w >> mouseResizeWindow w)
    ]

myLayout = mkToggle (FULL ?? EOT) BSP.emptyBSP

myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myStartupHook = do
    setLayout US
    return ()

main =
  do
    nScreens <- countScreens
    xmproc <- spawnPipe "xmobar"
    xmonad $
      ewmh $ docks def
        { terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          modMask = myModMask,
          borderWidth = myBorderWidth,
          workspaces = withScreens nScreens myWorkspaces,
          keys = myKeys,
          mouseBindings = myMouseBindings,
          layoutHook = avoidStruts $ myLayout,
          manageHook = myManageHook <+> manageDocks,
          logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        },
          startupHook = myStartupHook
        }

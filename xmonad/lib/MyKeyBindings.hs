module MyKeyBindings where

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Layout.BinarySpacePartition

import XMonad.Util.CustomKeys


myKeys = customKeys removedKeys addedKeys

removedKeys :: XConfig l -> [(KeyMask, KeySym)]
removedKeys XConfig {modMask = modm} =
    [ (modm              , xK_space)  -- Default for layout switching
    , (modm .|. shiftMask, xK_Return) -- Default for opening a terminal
    , (modm .|. shiftMask, xK_c)      -- Default for closing the focused window
    ]

addedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addedKeys conf @ XConfig {modMask = modm} =
  [ -- Application launcher
    ((modm, xK_space) , spawn "rofi -show")

    -- Terminal
  , ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- Close application
  , ((modm, xK_w), kill)

    -- Switch to last workspace
  , ((modm, xK_Tab), toggleWS)

    -- Rotate windows
  , ((modm, xK_r), sendMessage Rotate)

    -- Swap windows
  , ((modm, xK_t), sendMessage Swap)

    -- Layout switching
  , ((modm .|. shiftMask, xK_t), sendMessage NextLayout)

    -- Directional navigation of windows
  , ((modm, xK_l), windowGo R False)
  , ((modm, xK_h), windowGo L False)
  , ((modm, xK_k), windowGo U False)
  , ((modm, xK_j), windowGo D False)

    -- Go to workspace, show which one
  , ((modm, xK_1), sequence_ [toggleOrView "browse", spawn "notify-send \"browse\""])
  , ((modm, xK_2), sequence_ [toggleOrView "code"  , spawn "notify-send \"code\""  ])
  , ((modm, xK_3), sequence_ [toggleOrView "read"  , spawn "notify-send \"read\""  ])
  , ((modm, xK_4), sequence_ [toggleOrView "chat"  , spawn "notify-send \"chat\""  ])
  , ((modm, xK_5), sequence_ [toggleOrView "etc"   , spawn "notify-send \"etc\""   ])

    -- Expand and shrink windows
  , ((modm .|. controlMask,                xK_l), sendMessage $ ExpandTowards R)
  , ((modm .|. controlMask,                xK_h), sendMessage $ ExpandTowards L)
  , ((modm .|. controlMask,                xK_j), sendMessage $ ExpandTowards D)
  , ((modm .|. controlMask,                xK_k), sendMessage $ ExpandTowards U)
  , ((modm .|. controlMask .|. shiftMask , xK_l), sendMessage $ ShrinkFrom R)
  , ((modm .|. controlMask .|. shiftMask , xK_h), sendMessage $ ShrinkFrom L)
  , ((modm .|. controlMask .|. shiftMask , xK_j), sendMessage $ ShrinkFrom D)
  , ((modm .|. controlMask .|. shiftMask , xK_k), sendMessage $ ShrinkFrom U)

    -- Toggle redshift
  , ((modm .|. controlMask, xK_r), spawn "systemctl --user start redshift")
  , ((modm .|. controlMask, xK_e), spawn "systemctl --user stop redshift")

    -- Brightness control
  , ((shiftMask, 0x1008ff13), spawn "sudo /home/rleppink/.local/bin/tpb -i --binary 1")
  , ((shiftMask, 0x1008ff11), spawn "sudo /home/rleppink/.local/bin/tpb -d --binary 1")
  , ((0,         0x1008ff41), spawn "sudo /home/rleppink/.local/bin/tpb -t")

    -- XF86AudioMute
  , ((0, 0x1008ff12), spawn "amixer set Master toggle")

    -- XF86AudioRaiseVolume
  , ((0, 0x1008ff13), spawn "amixer set Master 5%+ -M")

    -- XF86AudioLowerVolume
  , ((0, 0x1008ff11), spawn "amixer set Master 5%- -M")

    -- Show date
  , ((modm, xK_a), spawn "notify-send \"$(date +%A\\,\\ %d\\ %B\\,\\ %R)\"")

    -- Show battery
  , ((modm, xK_s), spawn "notify-send \"$(cat /sys/class/power_supply/BAT0/status), $(cat /sys/class/power_supply/BAT0/capacity)%\"")

    -- Screenshots
  , ((0, xK_F10), spawn "maim ~/Pictures/$(date +%s).png")
  , ((0, xK_F9 ), spawn "maim -s | xclip -selection clipboard -t image/png")
  ]


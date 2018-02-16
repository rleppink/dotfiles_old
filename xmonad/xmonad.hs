import XMonad

import XMonad.Actions.Navigation2D

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing

import MyKeyBindings


main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridNavigation }
    $ myConfig


-- TODO: Get these colors from xrdb
backgroundColor   = "#FEFEFE"
middleColor       = "#AEAEAE"
foregroundColor   = "#0E0E0E"

myConfig = ewmh def
  { borderWidth        = 1
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = fullscreenEventHook
  , keys               = myKeys
  , layoutHook         = spacingWithEdge 2 emptyBSP
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , terminal           = "urxvt"
  , workspaces         = [ "browse", "code", "read", "chat", "etc" ]
  }


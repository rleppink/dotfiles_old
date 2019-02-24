import XMonad

import XMonad.Actions.Navigation2D

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import MyKeyBindings


main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridNavigation }
    $ myConfig


-- TODO: Get these colors from xrdb
backgroundColor   = "#FFFFFF"
middleColor       = "#AEAEAE"
foregroundColor   = "#000000"

myConfig = ewmh def
  { borderWidth        = 1
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = fullscreenEventHook
  , keys               = myKeys
  , layoutHook         =   spacingWithEdge 2 emptyBSP
                       ||| spacingWithEdge 2 (ThreeColMid 1 (3/100) (2/3))
                       ||| noBorders emptyBSP
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , terminal           = "urxvt"
  , workspaces         = [ "browse", "code", "read", "chat", "etc" ]
  }


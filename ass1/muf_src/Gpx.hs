module Gpx where

type ColourName = String
type Animation = [Frame]
type Frame = [GpxInstr]
data GpxInstr = DrawRect Integer Integer Integer Integer ColourName
              | DrawCirc Integer Integer Integer ColourName
              deriving (Eq, Show)

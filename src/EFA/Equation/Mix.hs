{-# LANGUAGE EmptyDataDecls #-}
module EFA.Equation.Mix where

data Source
data Sink

class Direction dir where switch :: f Source -> f Sink -> f dir
instance Direction Source where switch x _ = x
instance Direction Sink   where switch _ x = x

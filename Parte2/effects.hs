{- |
     Implementacion de una estructura Effects,
     con distintos efectos especiales para los Pixels
-}

module Effects (

  -- * El tipo @Effects@
  Effects (
     Say,
     Up, Down, Left, Right,
     UpsideDown, Backwards,
     Negative,
     Delay,
     Color,
     Repeat,
     Forever
     ),

  evalE, evalL
  ) where

import qualified Graphics.HGL as HGL (Color)
import qualified Data.Map as Map (Map)
import Control.Concurrent (threadDelay)
import Pixels as Pix

-- | Tipo de datos para Effects
data Effects = Say String
             | Up
             | Down
             | Left
             | Right
             | Backwards
             | UpsideDown
             | Negative
             | Delay Integer
             | Color HGL.Color
             | Repeat Integer [Effects]
             | Forever [Effects]
             deriving (Show,Read)

-- | 'evalE' Aplica el efecto al Pixels
evalE :: Effects -> Pixels -> Map.Map Char Pixels -> Int -> Pixels
evalE (Say s) p mc max = concatPixels $ map (font mc) $ s 
                         ++ (take (max - (length s)) (repeat ' '))
evalE Up p         _ _ = up p
evalE Down p       _ _ = down p
evalE Effects.Left p       _ _ = left p
evalE Effects.Right p      _ _ = right p
evalE Backwards p  _ _ = backwards p
evalE UpsideDown p _ _ = upsideDown p
evalE Negative p   _ _ = negative p
evalE (Color c) p  _ _ = p { color = c }
    
    
-- | 'evalL' funcion que arma la lista con los efectos a mostrar
evalL [] = []
evalL ((Forever xs):es) = cycle $evalL xs
evalL ((Repeat i xs):es) = (concat (replicate (fromInteger i ::Int) (evalL xs))) ++ (evalL es)
evalL (e:es) = e : (evalL es)
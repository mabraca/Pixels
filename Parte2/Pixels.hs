
{- |
     Implementacion de una estructura Pixels,
     que representa los carectares de la tabla ASCII
-}

module Pixels (
  -- * El tipo @Pixel@
  Pixel(Pixel),

  -- * Operacion que obtiene el valor Bool del Pixel
  on,

  -- * El tipo @Pixels@
  Pixels(Pixels),

  -- * Operaciones que obtiene el valor del Color y matriz de Pixels
  color, dots,

  -- * Operaciones para efectos especiales
  up, down, left, right, upsideDown, backwards, negative,
  
  -- * Operaciones para combinar pixels
  concatPixels, font, allOff
  
  
  ) where

import qualified Graphics.HGL as HGL (Color)
import qualified Data.Map as Map

-- | Tipo de datos para Pixels
data Pixels = Pixels { color :: HGL.Color, dots :: [[Pixel]] }
              deriving Show

-- | Tipo de datos para Pixel
data Pixel = Pixel { on :: Bool }
             deriving Show

-- | 'up' dezplaza una hilera hacia arriba
up :: Pixels -> Pixels
up (Pixels{color=c,dots=d}) = Pixels c (tail d ++ [head d])
   
-- | 'down' dezplaza una hilera hacia abajo
down :: Pixels -> Pixels
down (Pixels{color=c,dots=d}) = Pixels c (last d : init d)

-- | 'left' dezplaza una columna hacia la izquierda.
left :: Pixels -> Pixels
left (Pixels{color=c,dots=d}) = Pixels c [tail x ++ [head x] | x<-d]

-- | 'right' dezplaza una columna hacia la derecha.
right :: Pixels -> Pixels
right (Pixels{color=c,dots=d}) = Pixels c [last x : init x | x<-d]

-- | 'upsideDown' invierte el orden de las filas.
upsideDown :: Pixels -> Pixels
upsideDown (Pixels{color=c,dots=d}) = Pixels c (reverse d)

-- | 'backwards' invierte el orden de las columnas.
backwards :: Pixels -> Pixels
backwards (Pixels{color=c,dots=d}) = Pixels c (map reverse d)

-- | 'negative' intercambia blancos por astericos y viceversa.  
negative :: Pixels -> Pixels
negative (Pixels{color=c,dots=d}) = Pixels c (neg d)
  where neg p = map (map (\c->Pixel (not (on c)))) p

-- | 'concatPixels' representa la concatenacion horizontal de los elementos
-- de la lista original
concatPixels :: [Pixels] -> Pixels
concatPixels (p:ps) = Pixels (color p) $foldl mergePixels (dots p) (map dots ps)
  where mergePixels a b = map (\ps -> (fst ps)++(snd ps)) $ zip a b

-- | 'allOff' apaga todo los Pixel dentro de un Pixels        
allOff :: Pixels -> Pixels
allOff (Pixels{color=c,dots=d}) = Pixels c (turnOff d)
  where turnOff p = map (map (\c->Pixel False)) p

-- | 'font' permite obtener la representacion en Pixels de un Char
font :: Map.Map Char Pixels -> Char -> Pixels
font bm c = if Map.member c bm then bm Map.! c
            else bm Map.! '\0'
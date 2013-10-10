{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List hiding (transpose, sum)
import Text.PrettyPrint.Boxes
import System.Random
import Control.Monad

import Numeric.Algebra
import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)

import Quaternion
import Extensive

mkBox :: (FiniteSet a, FiniteSet b, Eq b, Eq a) 
      => V (Hom a b) -> Box
mkBox m = box
      where
        es = map return elements
        box = hsep 2 left cls
        cls = [ vsep 0 right (map (ts . snd) (coefficients (apply m e'))) | e' <- es]
        ts = text . show'

printMap :: (FiniteSet a, FiniteSet b, Eq b, Eq a) 
         =>  (V a -> V b) -> IO ()
printMap  = putStrLn . render . mkBox . hom


-- Make a random matrix
randomElement :: (FiniteSet a) => IO (V a)
randomElement = 
 do 
    let sce b = fmap (flip scale (return b)) (randomRIO (-10, 10) )
    cs <- mapM sce elements
    return $ foldl1 (+) cs

randomMatrix :: (FiniteSet a, Eq a, FiniteSet b, Eq b) => IO (V a -> V b)
randomMatrix = fmap apply randomElement

main :: IO ()
main = do 
    
    -- Generate random three by three linear transformation
    (a :: V H -> V H)  <- randomMatrix
    putStrLn "A = "
    printMap a
    
    let ainv = inverse a

    putStrLn "A^{-1} = "
    printMap ainv

    putStrLn "Check..."
    printMap (ainv . a)


    putStrLn "Done"




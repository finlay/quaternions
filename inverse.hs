{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List hiding (transpose, sum)
import System.Random
import Control.Monad

import Numeric.Algebra
import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)

import Quaternion
import Extensive


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
    (a :: V (Tensor H H) -> V (Tensor H H))  <- randomMatrix
    putStrLn "A = "
    printMap a
    
    let ainv = inverse a

    putStrLn "A^{-1} = "
    printMap ainv

    putStrLn "Check..."
    printMap (ainv . a)


    putStrLn "Done"




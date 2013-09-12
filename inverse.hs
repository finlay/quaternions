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

-- Create a rotation on an off diagonal for a endomorphism
rot :: Eq a => a -> a -> R -> V a -> V a
rot x' y' t = extend $ r'
  where 
    r' i' | x' == i'  =         (scale (cos t) (return x'))  + (scale (sin t) (return y'))
          | y' == i'  = (negate (scale (sin t) (return x'))) + (scale (cos t) (return y'))
          | otherwise = return i'


-- Given an off diagonal element, cacluate the angle
angle :: R -> R
angle ct = 
    let sgn a = a / abs a
    in atan $ (sgn ct) / ((abs ct) + (sqrt (1 + ct*ct)))

makeRotation :: Eq a => (V a -> V a) -> a -> a -> (V a -> V a)
makeRotation m x' y' = 
    let mc a b = unV (m (return a)) (delta b)
        ct = ((mc x' x') - (mc y' y')) / (2*(mc x' y'))
    in  rot x' y' (angle ct)

-- Diagonal element
data Diag a = Diag !a !a deriving (Eq, Show)
offdiag :: (FiniteSet a, Order a) => [ Diag a ]
offdiag = [ Diag x' y' | x' <- elements, y' <- elements, x' < y']

diagStep :: (FiniteSet a, Eq a) 
         => Diag a -> (V a -> V a) -> ((V a -> V a), (V a -> V a))
diagStep (Diag r s) m = 
        let !t' = makeRotation m r s
            !m' = (transpose t') `mmul` m `mmul` t'
        in (m', t')


-- Diagonalise a symmetric matrix
diagonaliseSym :: (FiniteSet a, Eq a, Order a) 
               => (V a -> V a) -> [(V a -> V a, V a -> V a)]
diagonaliseSym = 
  let dds = foldr1 (++) (repeat offdiag)
      go ds res (m, _) = 
        let (d:ds') = ds 
            (m', t') = diagStep d m
            res' = res ++ [(m', t')]
        in if   offNorm m' < 1e-8
           then res'
           else go ds' res' (m', t')
  in  go dds [] . (, id)

    
offNorm :: (FiniteSet a, Eq a, Order a) => (V a -> V a) -> R
offNorm m = 
  let mc (Diag a b) = unV (m (return a)) (delta b)
  in  sum $ map ((**2) . mc) offdiag


inverse :: (FiniteSet a, Eq a, Order a, FiniteSet b, Eq b, Order b)
        => (V a -> V b) -> V b -> V a
inverse l = 
    let ltl   = transpose l `mmul` l
        steps = diagonaliseSym ltl
        lt     = foldl1 mmul (map snd steps)
        d     = foldl1 (flip const) (map fst steps)
        V hd  = hom d 
        de s' = scale (1/(sqrt (hd (delta (Hom s' s'))))) (return (Hom s' s'))
        dinv  = apply $ sum [ de s | s <- elements ]
        rt = l `mmul` lt `mmul` dinv
        linv = lt `mmul` dinv `mmul` (transpose rt)
    in linv

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




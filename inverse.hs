{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import Text.Printf
import Data.List hiding (transpose)
import Test.QuickCheck hiding (elements)
import Text.PrettyPrint.Boxes
import System.Random
import Control.Monad

import Quaternion
import Extensive

-- Matrix type for 3 x 3 matricies
type Matrix = V SO3 -> V SO3 
mkBox :: Matrix -> Box
mkBox m = box
      where
        es = map return elements :: [ V SO3 ]
        box = hsep 2 left cols
        cols  = [ vsep 0 right (map (ts . snd) (coefficients (m e))) | e <- es]
        ts = text . show'
instance Show Matrix where
  show = render . mkBox

-- Make a rotation with given angle in xy plane
theta :: R
theta = 12.3*pi

r :: SO3 -> SO3 -> R -> V SO3 -> V SO3
r a b t = extend r'
  where 
    r' :: SO3 -> V SO3 
    r' i | a == i  =  (scale (cos t) (return a)) + (scale (sin t) (return b))
         | b == i  = -(scale (sin t) (return a)) + (scale (cos t) (return b))
         | otherwise  = return i


-- Given an off diagonal element, cacluate the angle
angle :: R -> R
angle ct = 
    let sgn a = a / abs a
    in atan $ (sgn ct) / ((abs ct) + (sqrt (1 + ct*ct)))

makeRotation :: Matrix -> SO3 -> SO3 -> Matrix
makeRotation m x y = 
    let mc a b = unV (m (return a)) (delta b)
        ct = ((mc x x) - (mc y y)) / (2*(mc x y))
    in  r x y (angle ct)

-- Diagonalise a symmetric matrix
offdiag = [ (x, y) | x <- elements, y <- elements, x < y] :: [(SO3, SO3)]
diagonaliseSym :: Matrix -> [(Matrix, Matrix)]
diagonaliseSym = unfoldr diag . (, id)
  where 
    diag :: (Matrix, Matrix) -> Maybe ((Matrix, Matrix), (Matrix, Matrix))
    diag (!m, !t) =       
        let mc a b = unV (m (return a)) (delta b)
            nonzero (x, y) = abs (mc x y) > epsilon
            nonzeros = filter nonzero offdiag
        in case nonzeros of 
            []      -> Nothing
            (x,y):_ -> 
                let  !t' = makeRotation m x y
                     !m' = (transpose t') . m . t'
                in   Just ((m', t . t'), (m', t . t'))
    
offNorm :: Matrix -> R
offNorm m = 
  let mc a b = unV (m (return a)) (delta b)
   in sum $ map ((**2) . uncurry mc) offdiag

-- Make a random matrix
randomElement :: IO (V SO3)
randomElement = 
 do 
    cs <- replicateM 3 (randomRIO (-10, 10))
    return $ foldl1 (+) $ map (uncurry scale) (zip cs [x, y, z])

randomMatrix :: IO Matrix
randomMatrix = 
  do
    elx <- randomElement 
    ely <- randomElement
    elz <- randomElement
    let m' :: SO3 -> V SO3
        m' X = elx
        m' Y = ely
        m' Z = elz
    return $ extend m'

main = do 
    
    -- Generate random three by three linear transformation
    a <- randomMatrix
    putStrLn "A = "
    print a
    
    -- Calculate two transposes A^TA and AA^T
    let ata = transpose a . a
    putStrLn "A^TA = "
    print ata

    let aat = a. transpose a
    putStrLn "AA^T = "
    print aat

    -- For each of those, caclulate rotations
    forM_ (take 15 $ diagonaliseSym ata) $ \( m, t) ->
        do  putStrLn $ render $ hsep 3 left [(mkBox m),  (mkBox t)]

    -- Put it all together

    putStrLn "Done"




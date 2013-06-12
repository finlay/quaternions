{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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
instance Show Matrix where
  show m = render box
      where
        es = map return elements :: [ V SO3 ]
        box = hsep 2 left cols
        cols  = [ vsep 0 right (map (ts . snd) (coefficients (m e))) | e <- es]
        ts = text . show'

-- Make a rotation with given angle in xy plane
theta :: R
theta = 12.3*pi
r :: R -> V SO3 -> V SO3
r t = extend r'
  where 
    r' X =  (scale (cos t) x) + (scale (sin t) y)
    r' Y = -(scale (sin t) x) + (scale (cos t) y)
    r' Z = z


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
    putStrLn $ show a
    
    -- Calculate two transposes A^TA and AA^T
    let ata = transpose a . a
    putStrLn "A^TA = "
    putStrLn $ show ata

    let aat = a. transpose a
    putStrLn "AA^T = "
    putStrLn $ show aat

    -- For each of those, caclulate rotations


    -- Put it all together

    putStrLn "Done"

{- 
 - # Calculating the inverse of a random linear transformation
 - 
 - Harder than it might seem.
 -
 - Plan is to follow the SVD decomposition. So that means, factoring out
 - orthogonal transformtions before and after, which leaves only a diagonal
 - matrix in the middle.
 -
 - Let A be a linear transformation.
 - Looking for V and W, orthogonal, and D diagonal, such that
 -   A = V D W
 -
 - Note that 
 -   A^TA = W^TD^TDW = W^T D^2 W
 - and similarly,
 -   AA^T = V D^2 V^T
 - 
 - which are both symmetric. What that means is that we can decompose to get V
 - and W separately, and simultaneously.
 -
 - If we can find a sequence of 
 -
 --}



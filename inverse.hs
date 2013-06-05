{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Text.Printf
import Data.List hiding (transpose)
import Test.QuickCheck hiding (elements)
import Text.PrettyPrint.Boxes

import Quaternion
import Extensive

type Matrix = V SO3 -> V SO3 

theta :: R
theta = 12.3*pi

r :: R -> V SO3 -> V SO3
r t = extend r'
  where 
    r' X =  (scale (cos t) x) + (scale (sin t) y)
    r' Y = -(scale (sin t) x) + (scale (cos t) y)
    r' Z = z

instance Show Matrix where
  show m = render box
      where
        es = map return elements :: [ V SO3 ]
        box = hsep 2 left cols
        cols  = [ vsep 0 right (map (ts . snd) (coefficients (m e))) | e <- es]
        ts = text . show'

main = do 
    putStrLn $ show (r theta)
    putStrLn $ show (transpose (r theta))
    putStrLn $ show (r theta . transpose (r theta))

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



{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Text.Printf
import Data.List
import Test.QuickCheck hiding (elements)

import Quaternion
import Extensive

so3 :: V SO3 -> V (Tau)
so3 = extend so3'
  where 
    so3' X = return $ Skew E I
    so3' Y = return $ Skew E J
    so3' Z = return $ Skew E K


prop_so3_lie_algebra_homomorphism :: V SO3 -> V SO3 -> Property
prop_so3_lie_algebra_homomorphism a b = 
    property $ so3 (a * b) == comm (so3 b) (so3 a)


main =  do
    --test5 elements elements
    quickCheck prop_so3_lie_algebra_homomorphism

test7 = do
    let xs = map return elements :: [V SO3]
        disp x = (printf " %s | " (show x)) 
                ++ (intercalate " | " (map (\y -> (printf "%4s" (show (x * y)))) xs ) )++ " |" 
    -- header
    putStrLn $ " [,]  | " ++ (intercalate " | " (map show xs)) ++ " |"
    putStrLn $ take 28 (repeat '-')
    mapM_ (putStrLn . disp) $ xs



test6 as bs = 
  let disp (x, y) = putStrLn $ printf "B(%8s, %8s) = %s" (show x) (show y) (show (killing x y))
      as' = map return as :: [V Tau]
      bs' = map return bs :: [V Tau]
      nonzero (x,y) = (killing x y) /= 0.0
  in  mapM_ disp $ filter nonzero [(x,y) | x <- as', y <- bs']

test5 as bs = 
  let disp (x, y) = putStrLn $ (printf "[%-8s, %8s] = %-25s" (show x) (show y) (show (comm x y)))
      as' = map return as :: [V Tau]
      bs' = map return bs :: [V Tau]
  in  mapM_ disp [(x,y) | x <- as', y <- bs']

test4 =
  let as = map show tau
      bs = map (show . injectTau) tau
      cs = map (show . injectTauInv . injectTau) tau
  in  mapM_ (\(a,b,c) -> putStrLn $ (printf "%-10s -> %-25s -> %-25s" a b c)) (zip3 as bs cs) 
    
test3 =
  let as = map show ehh
      bs = map (show . injectTauInv) ehh
      cs = map (show . injectTau . injectTauInv) ehh
  in  mapM_ (\(a,b,c) -> putStrLn $ (printf "%-10s -> %-25s -> %-25s" a b c)) (zip3 as bs cs) 
    
test2 =
  let as = map show ehh
      bs = map (show . injectTauInv) ehh
  in  mapM_ (putStrLn . uncurry (printf "%10s -> %-10s")) (zip as bs) 
    
test =
  let as = map show tau
      bs = map (show . injectTau) tau
  in  mapM_ (putStrLn . uncurry (printf "%10s -> %-10s")) (zip as bs) 


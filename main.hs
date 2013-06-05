{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Text.Printf
import Data.List
import Test.QuickCheck hiding (elements)
import Text.PrettyPrint.Boxes

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
    test8 (map return (sym0 ++ ske1 ++ sym2) :: [V Tau]) comm
    putStrLn " +++ "
    test8 (map return (sym0 ++ sym1 ++ ske2) :: [V Tau]) comm
    --quickCheck prop_so3_lie_algebra_homomorphism

data Hole = Hole

test8 es pr = printBox box
  where
    box = hsep 2 left cols
    topleft = emptyBox 1 5
    cols  = first : body
    first =   vsep 0 left  ( topleft : map ts es)
    body  = [ vsep 0 right ( ts e :    col e) | e <- es]
    col e = [ ts (pr e f) | f <- es ]
    ts = text . show
    

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


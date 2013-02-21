{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Text.Printf
import Data.List (unfoldr)
import Data.Maybe (fromJust)

import Extensive


data H = E | I | J | K deriving (Eq, Ord)
instance FiniteSet H where elements = [ E, I, J, K ]
instance Show H where
    show E = "e" ; show I = "i"
    show J = "j" ; show K = "k"

[e,i,j,k] = map return elements :: [V H]

mu :: V (Tensor H H) -> V H
mu = extend mu'
  where
    mu' :: Tensor H H -> V H
    mu' (E `Tensor` b) = return b
    mu' (b `Tensor` E) = return b
    mu' (I `Tensor` J) = k
    mu' (J `Tensor` K) = i
    mu' (K `Tensor` I) = j
    mu' (J `Tensor` I) = minus k
    mu' (K `Tensor` J) = minus i
    mu' (I `Tensor` K) = minus j
    mu' (I `Tensor` I) = minus e
    mu' (J `Tensor` J) = minus e
    mu' (K `Tensor` K) = minus e

instance Algebra (V H) where
    unit    = e
    mul x y = mu (x `tensor` y)

-- Now lets make Tensor H H an algebra
instance Algebra (V (Tensor H H)) where
    unit = e `tensor` e
    mul x y = extend muHH (x `tensor` y)
            where 
                muHH  (Tensor (Tensor x y) (Tensor x' y')) 
                    = ((return x) * (return x')) `tensor` ((return y') * (return y))

comm a b = a * b - b * a
ehh = map return elements :: [V (Tensor H H)]

-- Create a new more convenient basis for H Tensor H
-- Need to give names, and elements
-- Then, expand arbitary elements in the new basis
data TauBasis = Sym H H | Skew H H deriving (Ord, Eq)
instance Show TauBasis where
    show (Sym  a b) = show a ++ " \x2228 " ++ show b
    show (Skew a b) = show a ++ " \x2227 " ++ show b

sym0 = [ Sym  a a | a <- [E,I,J,K]]
sym1 = [ Sym  E a | a <- [I,J,K]]
sym2 = [ Sym  J K , Sym  K I, Sym  I J]
ske1 = [ Skew E a | a <- [I,J,K]]
ske2 = [ Skew J K , Skew K I, Skew I J]

instance FiniteSet TauBasis where
    elements =  sym0 ++ sym1 ++ sym2 ++ ske1 ++ ske2

instance Algebra (V TauBasis) where
    unit = return (Sym E E)
    mul x y = injectTauInv ((injectTau x) * (injectTau y))

tau = map return elements :: [V TauBasis]

injectTau :: V TauBasis -> V (Tensor H H)
injectTau = extend injectTau'
  where
    injectTau' (Sym  x y) =  let x' = return x 
                                 y' = return y
                             in  scale 0.5 (x' `tensor` y' + y' `tensor` x')
    injectTau' (Skew x y) =  let x' = return x 
                                 y' = return y
                             in  scale 0.5 (x' `tensor` y' - y' `tensor` x')

injectTauInv :: V (Tensor H H) -> V TauBasis
--injectTauInv = fromJust $ inverse injectTau
injectTauInv = extend injectTauInv'
  where
    injectTauInv' (E `Tensor` E) = return (Sym E E) 
    injectTauInv' (I `Tensor` I) = return (Sym I I)
    injectTauInv' (J `Tensor` J) = return (Sym J J)
    injectTauInv' (K `Tensor` K) = return (Sym K K)
    injectTauInv' (E `Tensor` I) = (return (Sym E I)) + (return (Skew E I))
    injectTauInv' (I `Tensor` E) = (return (Sym E I)) - (return (Skew E I))
    injectTauInv' (E `Tensor` J) = (return (Sym E J)) + (return (Skew E J))
    injectTauInv' (J `Tensor` E) = (return (Sym E J)) - (return (Skew E J))
    injectTauInv' (E `Tensor` K) = (return (Sym E K)) + (return (Skew E K))
    injectTauInv' (K `Tensor` E) = (return (Sym E K)) - (return (Skew E K))
    injectTauInv' (I `Tensor` J) = (return (Sym I J)) + (return (Skew I J))
    injectTauInv' (J `Tensor` I) = (return (Sym I J)) - (return (Skew I J))
    injectTauInv' (J `Tensor` K) = (return (Sym J K)) + (return (Skew J K))
    injectTauInv' (K `Tensor` J) = (return (Sym J K)) - (return (Skew J K))
    injectTauInv' (K `Tensor` I) = (return (Sym K I)) + (return (Skew K I))
    injectTauInv' (I `Tensor` K) = (return (Sym K I)) - (return (Skew K I))

-- Lets see how one element acts
--checkElement :: V x -> V x -> String
checkElement a b = 
    let bs = unfoldr (\b' -> let b'' = comm a b' in if b == b'' || b'' == 0 then Nothing else Just (b'', b'')) b 
    in  bs



test =
  let as = map show tau
      bs = map (show . injectTau) tau
  in  mapM_ (putStrLn . uncurry (printf "%10s -> %-10s")) (zip as bs) 

test2 =
  let as = map show ehh
      bs = map (show . injectTauInv) ehh
  in  mapM_ (putStrLn . uncurry (printf "%10s -> %-10s")) (zip as bs) 
    
test3 =
  let as = map show ehh
      bs = map (show . injectTauInv) ehh
      cs = map (show . injectTau . injectTauInv) ehh
  in  mapM_ (\(a,b,c) -> putStrLn $ (printf "%-10s -> %-25s -> %-25s" a b c)) (zip3 as bs cs) 
    
test4 =
  let as = map show tau
      bs = map (show . injectTau) tau
      cs = map (show . injectTauInv . injectTau) tau
  in  mapM_ (\(a,b,c) -> putStrLn $ (printf "%-10s -> %-25s -> %-25s" a b c)) (zip3 as bs cs) 
    
test5 as bs = 
  let disp (x, y) = putStrLn $ (printf "[%-8s, %8s] = %-25s" (show x) (show y) (show (comm x y)))
      as' = map return as :: [V TauBasis]
      bs' = map return bs :: [V TauBasis]
  in  mapM_ disp [(x,y) | x <- as', y <- bs']



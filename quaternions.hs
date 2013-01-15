{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Text.Printf
import Data.List (unfoldr)

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

sym0 = map (scale 0.5) $ [ x `tensor` x  | x <- [e,i,j,k]]
sym1 = map (scale 0.5) $ [ e `tensor` x + x `tensor` e | x <- [i,j,k]]
sym2 = map (scale 0.5) $ [ j `tensor` k + k `tensor` j, 
                           k `tensor` i + i `tensor` k, 
                           i `tensor` j + j `tensor` i ]
sym = sym0 ++ sym1 ++ sym2

ske1 = map (scale 0.5) $ [ e `tensor` x - x `tensor` e | x <- [i,j,k]]
ske2 = map (scale 0.5) $ [ j `tensor` k - k `tensor` j, 
                           k `tensor` i - i `tensor` k, 
                           i `tensor` j - j `tensor` i ]
ske = ske1 ++ ske2


showProd a b = 
    [ (printf "[%18s ,%18s ] = " (show x) (show y))  ++ show (comm x y) 
        | x <- a, y <- b, x < y]

showProdInBasis bs xs ys = 
    let sh = showInBasis bs
    in  [ (printf "[%10s ,%10s ] = " (sh x) (sh y))  ++ sh (comm x y) 
            | x <- xs, y <- ys]


-- Create a new more convenient basis for H Tensor H
-- Need to give names, and elements
-- Then, expand arbitary elements in the new basis
data TauBasis = Sym H H | Skew H H deriving (Ord, Eq)
instance Show TauBasis where
    show (Sym  a b) = show a ++ " \x2228 " ++ show b
    show (Skew a b) = show a ++ " \x2227 " ++ show b

instance FiniteSet TauBasis where
    elements =  [ Sym  a a | a <- [E,I,J,K]]
             ++ [ Sym  E a | a <- [I,J,K]]
             ++ [ Sym  J K , Sym  K I, Sym  I J]
             ++ [ Skew E a | a <- [I,J,K]]
             ++ [ Skew J K , Skew K I, Skew I J]

instance Basis TauBasis (Tensor H H) where
    --eta   :: TauBasis -> (Tensor H H) -> R
    eta (Sym  a b) (Tensor x y) = (delta a x * delta b y + delta b x * delta a y)
    eta (Skew a b) (Tensor x y) = (delta a x * delta b y - delta b x * delta a y)

tau = elements :: [TauBasis]


-- Lets see how one element acts
--checkElement :: V x -> V x -> String
checkElement a b = 
    let bs = unfoldr (\b' -> let b'' = comm a b' in if b == b'' || b'' == 0 then Nothing else Just (b'', b'')) b 
    in  bs





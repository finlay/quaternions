{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Extensive where

import Text.PrettyPrint.Boxes
import Control.Monad (join)
import Control.Monad.State
import Control.Applicative 
import Text.Printf

import Numeric.Natural.Internal
import Numeric.Algebra
import Prelude hiding ((+), (-), (*), (/), (^), negate, (>), (<), sum, fromInteger)
import qualified Prelude

import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC


--type R = Rational ; epsilon = 0 -- slow and accurate
type R = Double ; 
epsilon :: R
epsilon = 1e-6 -- fast and approximate 
show' :: forall t. (PrintfType (R -> t)) => R -> t
show' r = printf "%0.4f" $ if abs r < epsilon then 0 else r

instance Additive Double where
  (+) = (Prelude.+)
  --sinnum1p n r = (1 Prelude.+ toNatural n) * r

instance Abelian Double

orderOrd :: Ord a => a -> a -> Maybe Ordering
orderOrd a b = Just (compare a b)
instance (Ord a) => Order a where order = orderOrd

instance Division Double where
  recip = Prelude.recip
  (/) = (Prelude./)
  (\\) = undefined -- not sure what this is supposed to be
  (^) = (Prelude.^^)


pow1pIntegral :: (Division r, Integral n) => r -> n -> r
pow1pIntegral r n = r ^ (1 Prelude.+ n)
instance Multiplicative Double where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Semiring Double

instance LeftModule  Natural Double where (.*)   = (*) . fromIntegral
instance LeftModule  Integer Double where (.*)   = (*) . fromIntegral
instance RightModule Natural Double where m *. n = m * fromIntegral n
instance RightModule Integer Double where m *. n = m * fromIntegral n

instance Monoidal Double where 
  zero = 0
  sinnum n r = fromIntegral n * r

instance Group Double where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Unital Double where one = 1

fromNaturalNum :: Num r => Natural -> r
fromNaturalNum (Natural n) = Prelude.fromInteger n
instance Rig Double where fromNatural = fromNaturalNum

instance Ring Double  where fromInteger = Prelude.fromInteger


newtype V a = V { unV :: ((a -> R) -> R) }

instance Functor V where 
    fmap f (V xs) = V $ xs . (flip ((flip id) . f))

instance Monad V where
    return x      = V $ flip id x
    (V x) >>= y   = V $ x . flip ( unV . y )


-- Tensor products are just pairs
data Tensor a b = Tensor a b deriving (Eq, Ord)
tensor :: V a -> V b -> V (Tensor a b)
tensor tx ty =  (join . (fmap t') . t'') (Tensor tx ty)
    where
        t'' (Tensor x y) = fmap (Tensor x) y
        t'  (Tensor x y) = fmap (flip Tensor y) x


-- Hom represent linear maps.
data Hom a b = Hom a b deriving (Eq, Ord)

hom :: (FiniteSet a, FiniteSet b, Eq b) => (V a -> V b) -> V (Hom a b)
hom l = 
  let xs = elements
      coefs = coefficients . l . return
  in  sum [scale c (return (Hom x' y')) | x' <- xs, (y',c) <- coefs x']

apply :: (Eq a) => V (Hom a b) -> V a -> V b
apply = curry (unMaybe . fmap ex . uncurry tensor)
  where
    unMaybe = extend (maybe zero return)
    ex (Tensor (Hom x y) x') = if x == x' then Just y else Nothing

em :: (Eq a) => Hom a b -> V a -> V b
em (Hom x y) (V vx) = 
  let em' vy x' = if x == x' then vy y else 0
  in  V $ vx . em'

-- memoised multiplication
mmul :: (Eq a, FiniteSet a, Eq c, FiniteSet c) 
     => (V b -> V c) -> (V a -> V b) -> (V a -> V c)
mmul a b = apply . hom $ (a . b)



-- Finite sets can be listed, which is elements
class FiniteSet x where elements :: [ x ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (Tensor x y) where
    elements = [ Tensor a b | a <- elements, b <- elements ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (Hom x y) where
    elements = [ Hom a b | a <- elements, b <- elements ]

instance (Show x, Show y) => Show (Tensor x y) where
    show (Tensor x y) = show x ++ " \x2297 " ++ show y

instance (Show x, Show y) => Show (Hom x y) where
    show (Hom x y) = show x ++ " \x21A6 " ++ show y

delta :: (Eq x) => x -> x -> R
delta a b = if a == b then 1 else 0

-- If we have a vector over a finite set, we can calculate the coefficients
coefficients :: (FiniteSet x, Eq x) => V x -> [(x, R)]
coefficients (V v) = map (\e -> (e, v (delta e))) elements

--instance (Eq a, FiniteSet a) => Eq (a -> R) where
--    x == y = all (\e -> x e == y e) elements

instance (Eq a, FiniteSet a) => Eq (V a) where
    x' == y' = sum (map (squared . snd) ( coefficients (subtract x' y'))) <= epsilon
              where 
                squared x'' = x'' * x''
                subtract (V x'') (V y'') = V $ (\ar -> x'' ar - y'' ar)

instance (Eq a, FiniteSet a, Ord a) => Ord (V a) where
    compare x y = compare (coefficients x) (coefficients y)


-- Now we can talk about maps
-- Extending a map into a the Vector space is easy peasy using the Monad instance
-- Automatically linear
extend :: (Monad m) => (a -> m b) -> m a -> m b
extend = flip (>>=)

codual :: (Eq a) => V a -> (a -> R)
codual (V x)  = x . delta

dual :: (FiniteSet a, Eq a) => (a -> R) -> V a
--dual x = foldl1 plus $ map (\e -> scale (x e) (return e))  elements
dual x = V (\y -> sum $ map (\e -> x e * y e) elements)

dot :: (Eq a, FiniteSet a) => V a -> V a -> R
dot (V y) = y . codual

-- Transpose
transpose :: (FiniteSet a, FiniteSet b, Eq a, Eq b) 
          => (V a -> V b) -> (V b -> V a)
transpose lm = dual . flip (unV . lm . return) . codual


-- --------------------
-- Inverses
-- uses a Jakobi approach

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
         => Diag a -> (V a -> V a) -> (V a -> V a, V a -> V a)
diagStep (Diag r s) m = 
        let !t' = makeRotation m r s
            !m' = (transpose t') `mmul` m `mmul` t'
        in (m', t')

data DiagState a = DiagState { diags       :: [Diag a]
                             , transforms  :: [V a -> V a]
                             , result      :: V a -> V a 
                             , count       :: Double
                             , diagsTotal  :: Int
                             , diagsLeft   :: Int
                             } deriving Show
nextDiagStep :: (FiniteSet a, Eq a) => State (DiagState a) ()
nextDiagStep =
    let mc ma (Diag a b) = unV (ma (return a)) (delta b)
    in do
        s <- get
        let d:ds = diags s
        let ma   = result s
        if (abs $ mc ma d) > 1e-8 
            then do
                let (m, t) = diagStep d ma
                ts <- gets transforms
                put $ s { diags      = ds 
                        , transforms = t:ts
                        , result     = m
                        , count      = count s + 1
                        , diagsLeft  = diagsTotal s
                        }
            else do 
                let diags_left = diagsLeft s - 1
                if diags_left Prelude.> 0 
                    then do
                        put $ s { diags     = ds 
                                , diagsLeft = diags_left - 1
                                }
                        nextDiagStep
                    else 
                        return ()

-- Diagonalise a symmetric matrix
diagonaliseSym :: (FiniteSet a, Eq a, Order a) 
               => Double -> (V a -> V a) -> ((V a -> V a, [V a -> V a]), DiagState a)
diagonaliseSym maxcount ma = 
    let ds = offdiag
        dds = foldr1 (++) (repeat ds)
        initialState = DiagState { diags      = dds
                                 , transforms = [id]
                                 , result     = ma
                                 , count      = 0
                                 , diagsTotal = length ds
                                 , diagsLeft  = length ds
                                 }
        diagonalise = do
            m <- gets result
            c <- gets count
            if c < maxcount && offNorm m > 1e-8 
                then nextDiagStep >> diagonalise
                else do 
                    m  <- gets result
                    ts <- gets transforms
                    return (m, ts)
    in  runState diagonalise initialState
       

offNorm :: (FiniteSet a, Eq a, Order a) => (V a -> V a) -> R
offNorm m = 
  let mc (Diag a b) = unV (m (return a)) (delta b)
  in  sum $ map ((**2) . mc) offdiag


inverse' :: (FiniteSet a, Eq a, Order a, FiniteSet b, Eq b, Order b)
        => (V a -> V b) -> (V b -> V a, DiagState a)
inverse' l = 
    let ltl   = transpose l `mmul` l
        (steps, ds) = diagonaliseSym 1000 ltl
        lt    = foldl1 mmul (reverse $ snd steps)
        d     = fst steps
        V hd  = hom d 
        de s' = scale (1/(sqrt (hd (delta (Hom s' s'))))) (return (Hom s' s'))
        dinv  = apply $ sum [ de s | s <- elements ]
        rt    = l `mmul` lt `mmul` dinv
        linv  = lt `mmul` dinv `mmul` (transpose rt)
    in (linv, ds)
inverse :: (FiniteSet a, Eq a, Order a, FiniteSet b, Eq b, Order b)
        => (V a -> V b) -> V b -> V a
inverse = fst . inverse'

-- Basis
showInBasis :: (Show b, Eq b) => [b] -> V b -> String
showInBasis bs v =
        let coef (V v) = v . delta
            pairs = map (\e -> (e, coef v e)) bs
            showPair (b, n) 
               | n == " + 1" = " + "  ++ show b
               | n == " - 1" = " - "  ++ show b
               | otherwise   = n      ++ show b
            showN (b, n') = let n = (read $ printf "%0.5f" n' ) :: Double
                                i = n == fromInteger (round n)
                                sh = if i then show . round else show 
                            in (b, if n > 0 then " + " ++ sh n else " - " ++ sh (abs n))
        in  case map (showPair . showN) . filter (\(_,n) -> n /= 0.0) $ pairs of 
                  [] -> " 0"
                  ss -> concat ss


instance (Eq a, FiniteSet a, Show a) => Show (V a) where
    show = let sh :: (Show a, Eq a) => [a] -> V a -> String
               sh = showInBasis 
           in  sh elements

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
instance (FiniteSet a, FiniteSet b, Eq b, Eq a) => Show (V a -> V b) where
    show = render. mkBox . hom

instance Monoidal (V a) where
    zero = V $ \_ -> 0

minus :: V a -> V a
minus (V x) = V $ negate . x 

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)

instance LeftModule  Natural (V a) where n .* m = scale (fromIntegral n) m
instance LeftModule  Integer (V a) where n .* m = scale (fromIntegral n) m
instance RightModule Natural (V a) where m *. n = scale (fromIntegral n) m
instance RightModule Integer (V a) where m *. n = scale (fromIntegral n) m

scale :: R  -> V a -> V a
scale r (V x) = V $ (r *) . x 

instance Group (V a) where
  x - y = plus x (minus y)
  negate = minus
  subtract x y = plus (minus x) y
  times n r = scale (fromIntegral n) r


instance Additive (V v) where
    (+)   = plus 

--  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Tensor a b) where
    arbitrary = Tensor <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Hom a b) where
    arbitrary = Hom    <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a) => Arbitrary (V a)
  where
    arbitrary = 
      do
        bs    <- QC.listOf1 QC.arbitrary
        coefs <- QC.vector (length bs)
        return $ foldl1 plus $ map (\(n,b) -> scale n (return b)) $ zip coefs bs



-- Working around the terrible Num class here
-- instance Algebra (V v) => Num (V v) where
--     (+) v w        = plus v w
--     (-) v w        = plus v (minus w)
--     negate w       = minus w
--     (*) v w        = mul v w
--     abs            = undefined
--     signum         = undefined
--     fromInteger i  = scale (fromInteger i) unit
-- 

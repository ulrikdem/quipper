{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Define various analytic functions for 'FDouble' and 'QDouble'.
module Quipper.Algorithms.QLS.RealFunc where

import Data.List(mapAccumL)
import Quipper
import Quipper.Algorithms.QLS.CircLiftingImport
import Quipper.Algorithms.QLS.QSignedInt
import Quipper.Algorithms.QLS.QDouble
import Quipper.Algorithms.QLS.Utils


-- * Some analytic functions on 'FDouble'.

-- | Approximation of the sine function using Taylor series.

approx_sin :: FDouble -> FDouble
approx_sin x = let x2 = x * x in
               let x3 = x2 * x in
               let x4 = x2 * x2 in
               let x5 = x4 * x in
               let x7 = x2 * x5 in
               let x9 = x2 * x7 in
               let x11 = x2 * x9 in
               x - (x3 / 6.0) 
                 + (x5 / 120.0) 
                 - (x7 / 5040.0) 
                 + (x9 / 362880.0) 
                 - (x11 / 39916800.0) 



{-# LINE 34 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| approx_sin :: FDouble -> FDouble
                      approx_sin x = let x2 = x * x in
                                     let x3 = x2 * x in
                                     let x4 = x2 * x2 in
                                     let x5 = x4 * x in
                                     let x7 = x2 * x5 in
                                     let x9 = x2 * x7 in
                                     let x11 = x2 * x9 in
                                     x - (x3 / 6.0) 
                                       + (x5 / 120.0) 
                                       - (x7 / 5040.0) 
                                       + (x9 / 362880.0) 
                                       - (x11 / 39916800.0) 
                      
                      
                      
 |] ) 
{-# LINE 35 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Implementation of the sine function valid on the whole domain of
-- 'FDouble'.

local_sin :: FDouble -> FDouble
local_sin x = let n = fromIntegral $ floor (x/(2.0*local_pi)) in
              let y = x - 2.0*local_pi*n in 
              if (y < local_pi/2.0) then approx_sin y
              else if (y > 3.0*local_pi/2.0) then approx_sin (y - 2.0*local_pi)
              else approx_sin (local_pi - y)



{-# LINE 45 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_sin :: FDouble -> FDouble
                      local_sin x = let n = fromIntegral $ floor (x/(2.0*local_pi)) in
                                    let y = x - 2.0*local_pi*n in 
                                    if (y < local_pi/2.0) then approx_sin y
                                    else if (y > 3.0*local_pi/2.0) then approx_sin (y - 2.0*local_pi)
                                    else approx_sin (local_pi - y)
                      
                      
                      
 |] ) 
{-# LINE 46 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Implementation of the cosine function valid on the whole domain
-- of 'FDouble'.

local_cos :: FDouble -> FDouble
local_cos x = local_sin (x + local_pi/2.0)





{-# LINE 54 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_cos :: FDouble -> FDouble
                      local_cos x = local_sin (x + local_pi/2.0)
                      
                      
                      
                      
                      
 |] ) 
{-# LINE 55 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- listAngle :: [Double]
listAngle = snd $ mapAccumL (\a x -> (a+1, x / (2 ** a))) 3.0 (replicate after_radix_length pi)

-- template_listAngle :: Circ [QDouble]
template_listAngle = mapM (\x -> qinit $ fdouble x) listAngle

-- listCos :: [Double]
listCos = map cos listAngle

-- template_listCos :: Circ [QDouble]
template_listCos = mapM (\x -> qinit $ fdouble x) listCos

-- listSin :: [Double]
listSin = map sin listAngle

-- template_listSin :: Circ [QDouble]
template_listSin = mapM (\x -> qinit $ fdouble x) listCos



-- list_values :: [(FDouble,FDouble,FDouble)]
list_values = map (\(x,y,z) -> (fdouble x, fdouble y, fdouble z)) $ 
                                    zip3 listAngle listCos listSin

template_list_values = mapM (\(x,y,z) -> do
                              x' <- qinit $ fdouble x
                              y' <- qinit $ fdouble y
                              z' <- qinit $ fdouble z
                              return (x',y',z')) $ zip3 listAngle listCos listSin


-- | Auxiliary function for 'local_sqrt'.

approx_sqrt :: Int -> FDouble -> FDouble 
approx_sqrt n x = case n of
                    0 -> x
                    n -> let s = approx_sqrt (paramPred n) x in (s + x/s)/2.0


{-# LINE 92 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| approx_sqrt :: Int -> FDouble -> FDouble 
                      approx_sqrt n x = case n of
                                          0 -> x
                                          n -> let s = approx_sqrt (paramPred n) x in (s + x/s)/2.0
                      
                      
 |] ) 
{-# LINE 93 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Approximation of the square root using iterative means.

local_sqrt :: FDouble -> FDouble
local_sqrt x = approx_sqrt paramTen x



{-# LINE 98 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_sqrt :: FDouble -> FDouble
                      local_sqrt x = approx_sqrt paramTen x
                      
                      
                      
 |] ) 
{-# LINE 99 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | The function 'Data.Complex.magnitude' defined for
-- 'FDouble'. Calculate the non-negative magnitude of a complex number.

local_mag :: FDouble -> FDouble -> FDouble
local_mag x y = local_sqrt (x * x + y * y)



{-# LINE 105 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_mag :: FDouble -> FDouble -> FDouble
                      local_mag x y = local_sqrt (x * x + y * y)
                      
                      
                      
 |] ) 
{-# LINE 106 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Apply the matrix
-- 
-- >  ( a b )
-- >  ( c d )
-- 
-- to the column vector (/x/,/y/).

rotate :: FDouble -> FDouble -> FDouble -> FDouble -> FDouble -> FDouble -> (FDouble,FDouble)
rotate a b c d x y = (a * x + b * y, c * x + d * y)
  where
    -- To help the GHC 8.0 typechecker
    dummy = id_fdouble x



{-# LINE 119 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| rotate :: FDouble -> FDouble -> FDouble -> FDouble -> FDouble -> FDouble -> (FDouble,FDouble)
                      rotate a b c d x y = (a * x + b * y, c * x + d * y)
                        where
                          
                          dummy = id_fdouble x
                      
                      
                      
 |] ) 
{-# LINE 120 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Auxiliary function for 'approx_atan2'.

approx_atan2_aux :: FDouble -> FDouble -> (FDouble,FDouble, FDouble) -> (FDouble, FDouble, FDouble)
                -> (FDouble,FDouble,FDouble)
approx_atan2_aux x y (angle, x', y') (r, cn, sn) =
    let (a,(b,c)) = if (y' > y) then (angle - r, rotate cn sn (-sn) cn x' y')
                    else (angle + r, rotate cn (-sn) sn cn x' y')
    in (a,b,c)
  where
    -- To help the GHC 8.0 typechecker
    dummy_r = id_fdouble r


{-# LINE 131 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| approx_atan2_aux :: FDouble -> FDouble -> (FDouble,FDouble, FDouble) -> (FDouble, FDouble, FDouble)
                                      -> (FDouble,FDouble,FDouble)
                      approx_atan2_aux x y (angle, x', y') (r, cn, sn) =
                          let (a,(b,c)) = if (y' > y) then (angle - r, rotate cn sn (-sn) cn x' y')
                                          else (angle + r, rotate cn (-sn) sn cn x' y')
                          in (a,b,c)
                        where
                          
                          dummy_r = id_fdouble r
                      
                      
 |] ) 
{-# LINE 132 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Definition of 'atan2' using a CORDIC method.  Assume (/x/,/y/) is
-- in first quadrant and that /x/ > /y/.

approx_atan2 :: FDouble -> FDouble -> FDouble
approx_atan2 y x = 
   let list = list_values in
   let (a,_,_) = foldl (approx_atan2_aux x y) (0.0, local_mag x y, 0.0) list in a


{-# LINE 139 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| approx_atan2 :: FDouble -> FDouble -> FDouble
                      approx_atan2 y x = 
                         let list = list_values in
                         let (a,_,_) = foldl (approx_atan2_aux x y) (0.0, local_mag x y, 0.0) list in a
                      
                      
 |] ) 
{-# LINE 140 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | Definition of 'atan2' using a CORDIC method. /x/ and /y/ can be any 'FDouble'.

local_atan2 :: FDouble -> FDouble -> FDouble
local_atan2 y' x' = 
   let (x,y,(pad,sign)) = if      (x' >= 0.0 && y' >= 0.0) then ( x',  y', (0.0, 1.0))
                          else if (x' >= 0.0 && y' <  0.0) then ( x', -y', (0.0, -1.0))
                          else if (x' <  0.0 && y' <  0.0) then (-x', -y', (-local_pi, 1.0))
                          else                                  (-x',  y', (local_pi,  -1.0))
   in
   let angle = if (x > y) then approx_atan2 y x
               else            local_pi/2.0 - approx_atan2 x y
   in sign * angle + pad



{-# LINE 153 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_atan2 :: FDouble -> FDouble -> FDouble
                      local_atan2 y' x' = 
                         let (x,y,(pad,sign)) = if      (x' >= 0.0 && y' >= 0.0) then ( x',  y', (0.0, 1.0))
                                                else if (x' >= 0.0 && y' <  0.0) then ( x', -y', (0.0, -1.0))
                                                else if (x' <  0.0 && y' <  0.0) then (-x', -y', (-local_pi, 1.0))
                                                else                                  (-x',  y', (local_pi,  -1.0))
                         in
                         let angle = if (x > y) then approx_atan2 y x
                                     else            local_pi/2.0 - approx_atan2 x y
                         in sign * angle + pad
                      
                      
                      
 |] ) 
{-# LINE 154 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
-- | The function 'Data.Complex.mkPolar' defined for 'FDouble'. Form a
-- complex number from polar components of magnitude and phase.

local_mkPolar :: FDouble -> FDouble -> (FDouble,FDouble)
local_mkPolar p t = (p * local_cos t, p * local_sin t)







{-# LINE 164 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
$( decToCircMonad [d| local_mkPolar :: FDouble -> FDouble -> (FDouble,FDouble)
                      local_mkPolar p t = (p * local_cos t, p * local_sin t)
                      
                      
                      
                      
                      
                      
                      
 |] ) 
{-# LINE 165 "Quipper/Algorithms/QLS/RealFunc.hs" #-}
instance Floating FDouble where
  pi    = fromRational $ toRational pi
  sin x = local_sin x
  cos x = local_cos x

  sinh x = (exp x - exp (-x)) / 2
  cosh x = (exp x - exp (-x)) / 2

  asinh x = error "asinh not defined for FDouble"
  acosh x = error "acosh not defined for FDouble"
  atanh x = error "atanh not defined for FDouble"

  exp x = undefined
  log x = undefined

  asin x = error "asin not defined for FDouble"
  acos x = error "acos not defined for FDouble"
  atan x = atan2 x 1


instance RealFloat FDouble where
   floatRadix _ = 2
   floatDigits x = xdouble_length x
   floatRange _ = (0,0)
   decodeFloat (XDouble k n) = (integer_of_fsint n, -k)
   encodeFloat x k = XDouble (-k) (fromInteger x)
   isNaN _ = False
   isInfinite _ = False
   isDenormalized _ = False
   isNegativeZero _ = False
   isIEEE _ = False
   atan2 y x = local_atan2 y x




-- | A type class for quantum floating-point numbers.
class QFloating a where
  -- | Quantum implementation of the sine function.
  q_sin :: a -> Circ a
  -- | Quantum implementation of the cosine function.
  q_cos :: a -> Circ a

instance QFloating QDouble where
  q_sin x = (unpack template_local_sin) x
  q_cos x = (unpack template_local_cos) x


-- | Quantum implementation of 'atan2' on 'QDouble'.
q_atan2 :: QDouble -> QDouble -> Circ QDouble
q_atan2 = unpack template_local_atan2

-- | Quantum implementation of 'Data.Complex.magnitude' on 'QDouble'.
q_magnitude :: (QDouble, QDouble) -> Circ QDouble
q_magnitude = Prelude.uncurry $ unpack template_local_mag


-- | Quantum implementation of 'Data.Complex.mkPolar' on 'QDouble'.
q_mkPolar :: QDouble -> QDouble -> Circ (QDouble,QDouble)
q_mkPolar = unpack template_local_mkPolar


-- | Quantum implementation of 'Data.Complex.realPart' on 'QDouble':
-- return the real part of a complex number. A quantum complex is a
-- pair of two 'QDouble's.
q_Re :: (QDouble,QDouble) -> Circ QDouble
q_Re (x,y) = return x

-- | Quantum implementation of 'Data.Complex.imagPart' on 'QDouble':
-- return the imaginary part of a copmlex number. A quantum complex is
-- a pair of two 'QDouble's.
q_Im :: (QDouble,QDouble) -> Circ QDouble
q_Im (x,y) = return y



my_test_fdouble = do
          for 0 37 1 $ \i -> do
            let x = fromIntegral i
            let a1 = fromRational $ toRational (sin(x * pi/37))
            let a2 = fromRational $ toRational (cos(x * pi/37))
            let z1 = local_atan2 a1 a2
            let z2 = fromRational $ toRational $ atan2  (sin(x * pi/37)) (cos(x * pi/37))
            putStrLn $ show_fdouble $ abs (z1 - z2)




-- * Template subroutines of analytic functions.

-- | Template version of 'sin'.
template_sin :: Circ (QDouble -> Circ QDouble)
template_sin = return $ \x -> box "sin" q_sin x

-- | Template version of 'cos'.
template_cos :: Circ (QDouble -> Circ QDouble)
template_cos = return $ \x -> box "cos" q_cos x

-- | Template version of 'atan2'.
template_atan2 :: Circ (QDouble -> Circ (QDouble -> Circ QDouble))
template_atan2 = return $ \x -> return $ \y -> box "atan" (uncurry q_atan2) (x,y)






-- * Template subroutines for dealing with quantum complex number encoded as pairs of 'QDouble'.

-- | Template version of 'Data.Complex.mkPolar'.
template_mkPolar :: Circ (QDouble -> Circ (QDouble -> Circ (QDouble,QDouble)))
template_mkPolar = return $ \x -> return $ \y -> box "mkPolar" (uncurry q_mkPolar) (x,y)

-- | Template version of the constructor 'Data.Copmlex.:+' of
-- 'Data.Complex.Complex'.
template_symb_colon_symb_plus_ :: Circ (QDouble -> Circ (QDouble -> Circ (QDouble,QDouble)))
template_symb_colon_symb_plus_ = return $ \x -> return $ \y -> return (x,y) 


-- | Template version of 'Data.Complex.magnitude'.
template_magnitude :: Circ ((QDouble,QDouble) -> Circ QDouble)
template_magnitude = return $ \p -> box "mag" q_magnitude p

-- | Template version of 'Data.Complex.realPart'.
template_realPart :: Circ ((QDouble,QDouble) -> Circ QDouble)
template_realPart = return $ \(x,y) -> return x

-- | Template version of 'Data.Complex.imagPart'.
template_imagPart :: Circ ((QDouble,QDouble) -> Circ QDouble)
template_imagPart = return $ \(x,y) -> return y


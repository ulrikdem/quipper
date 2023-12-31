{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LINE 1 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
-- | Helper module for "Quipper.Algorithms.QLS.QSignedInt": some functions
-- defined with Template Haskell.
module Quipper.Algorithms.QLS.QSignedIntAux where

import Quipper
import Quipper.Libraries.Arith



-- | Subtraction on lists of booleans, understood as big-headian,
-- unsigned integers.

-- void definition for now
be_boollist_sub :: [Bool] -> [Bool] -> [Bool]
be_boollist_sub = error "be_boollist_sub yet undefined."


-- Template version of 'be_boollist_sub'.

-- Right now, call to q_sub.
-- template_be_boollist_sub :: Circ ([Qubit] -> Circ ([Qubit] -> Circ [Qubit]))
template_be_boollist_sub = return $ \x -> return $ \y -> do
                let x' = qdint_of_qulist_bh x
                let y' = qdint_of_qulist_bh y
                (_,_,z) <- q_sub x' y'
                return $ qulist_of_qdint_bh z


-- | Addition on lists of booleans, understood as big-headian, unsigned
-- integers.

-- void definition for now.
be_boollist_add :: [Bool] -> [Bool] -> [Bool]
be_boollist_add = error "be_boollist_add yet undefined."



-- Template version of 'be_boollist_add'

-- Right now, call q_add
-- template_be_boollist_add :: Circ ([Qubit] -> Circ ([Qubit] -> Circ [Qubit]))
template_be_boollist_add = return $ \x -> return $ \y -> do
                let x' = qdint_of_qulist_bh x
                let y' = qdint_of_qulist_bh y
                (_,_,z) <- q_add x' y'
                return $ qulist_of_qdint_bh z



-- | Strict ordering on lists of booleans, understood as big-headian
-- unsigned integers. If the lists are not of equal length, the
-- shorter list is treated as if its tail were padded with zeros.

be_boollist_less :: [Bool] -> [Bool] -> Bool
be_boollist_less l1 l2 = 
  case (l1,l2) of
    (a,[]) -> False
    ([],a)  -> be_boollist_less [False] a
    (h1:t1, h2:t2) -> 
       let parity = if h1 then h2 else not h2 in
       if parity then be_boollist_less t1 t2
       else h2



{-# LINE 64 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
$( decToCircMonad [d| be_boollist_less :: [Bool] -> [Bool] -> Bool
                      be_boollist_less l1 l2 = 
                        case (l1,l2) of
                          (a,[]) -> False
                          ([],a)  -> be_boollist_less [False] a
                          (h1:t1, h2:t2) -> 
                             let parity = if h1 then h2 else not h2 in
                             if parity then be_boollist_less t1 t2
                             else h2
                      
                      
                      
 |] ) 
{-# LINE 65 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
-- | Strict ordering on lists of booleans, understood as big-headian
-- signed integers: the 'Bool' in the pair stands for the sign: 'False'
-- is positive, 'True' is negative.

be_signed_boollist_less :: (Bool,[Bool]) -> (Bool,[Bool]) -> Bool
be_signed_boollist_less (b1,l1) (b2,l2) = 
    if b1 then (if b2 then be_boollist_less l2 l1  else True)
    else       (if b2 then False else be_boollist_less l1 l2)



{-# LINE 74 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
$( decToCircMonad [d| be_signed_boollist_less :: (Bool,[Bool]) -> (Bool,[Bool]) -> Bool
                      be_signed_boollist_less (b1,l1) (b2,l2) = 
                          if b1 then (if b2 then be_boollist_less l2 l1  else True)
                          else       (if b2 then False else be_boollist_less l1 l2)
                      
                      
                      
 |] ) 
{-# LINE 75 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
-- | Test whether all elements of a list are 'False'.

boollist_is_zero :: [Bool] -> Bool
boollist_is_zero l =
   case l of
     []    -> True
     (h:t) -> if h then boollist_is_zero t else False



{-# LINE 83 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
$( decToCircMonad [d| boollist_is_zero :: [Bool] -> Bool
                      boollist_is_zero l =
                         case l of
                           []    -> True
                           (h:t) -> if h then boollist_is_zero t else False
                      
                      
                      
 |] ) 
{-# LINE 84 "Quipper/Algorithms/QLS/QSignedIntAux.hs" #-}
-- | Addition on signed integers, encoded as big-headian lists of
-- booleans.

be_signed_boollist_add :: (Bool,[Bool]) -> (Bool,[Bool]) -> (Bool,[Bool])
be_signed_boollist_add (b,x) (c,y) =
    let parity = if b then c else not c in
    let (d,z) = if parity then (b, be_boollist_add x y)
                else if (be_boollist_less x y) then (c, be_boollist_sub y x)
                else (b, be_boollist_sub x y)
    in if (boollist_is_zero z) then (False,z) else (d,z)

$( decToCircMonad [d| be_signed_boollist_add :: (Bool,[Bool]) -> (Bool,[Bool]) -> (Bool,[Bool])
                      be_signed_boollist_add (b,x) (c,y) =
                          let parity = if b then c else not c in
                          let (d,z) = if parity then (b, be_boollist_add x y)
                                      else if (be_boollist_less x y) then (c, be_boollist_sub y x)
                                      else (b, be_boollist_sub x y)
                          in if (boollist_is_zero z) then (False,z) else (d,z)
                      
 |] ) 

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides miscellaneous general-purpose auxiliary
-- functions used by the "Graphics.EasyRender" package.

module Graphics.EasyRender.Auxiliary where

-- ----------------------------------------------------------------------
-- * Arithmetic operations
  
-- | A version of the 'ceiling' function that returns an 'Integer'.
int_ceiling :: RealFrac a => a -> Integer
int_ceiling = ceiling

-- ----------------------------------------------------------------------
-- * The Curry type class

-- | The 'Curry' type class is used to implement functions that have a
-- variable number of arguments. It provides a family of type
-- isomorphisms
-- 
-- @fun  ≅  args -> res,@
-- 
-- where
-- 
-- > fun = a1 -> a2 -> ... -> an -> res,
-- > args = (a1, (a2, (..., (an, ())))).

class Curry fun args res | args res -> fun where
  -- | Multiple curry: map a function 
  -- (/a/₁, (/a/₂, (…, ())) → /b/ 
  -- to its curried form 
  -- /a/₁ → /a/₂ → … → /b/.
  mcurry :: (args -> res) -> fun
  -- | Multiple uncurry: map a function
  -- /a/₁ → /a/₂ → … → /b/
  -- to its uncurried form 
  -- (/a/₁, (/a/₂, (…, ())) → /b/.
  muncurry :: fun -> (args -> res)
               
instance Curry b () b where
  mcurry g = g ()
  muncurry x = const x

instance Curry fun args res => Curry (a -> fun) (a,args) res where
  mcurry g x = mcurry (\xs -> g (x,xs))
  muncurry f (x,xs) = muncurry (f x) xs
                

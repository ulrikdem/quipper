{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module is for use with "Quipper.Utils.Template.Lifting". 
-- It contains various lifted functions of general use. They are not
-- intended to be used directly (although this would not break
-- anything).

module Quipper.Utils.Template.Auxiliary where

import Quipper.Utils.Auxiliary (fold_right_zip,fold_right_zipM)
import Data.List
import Control.Monad

newtype Param a = Param {getParam :: a}

instance Functor Param where
  fmap f (Param x) = Param (f x)

instance Applicative Param where
  pure = Param
  Param f <*> Param x = Param (f x)

instance Monad Param where
  Param x >>= f = f x

instance Foldable Param where
  foldMap f (Param x) = f x

instance Traversable Param where
  sequenceA (Param x) = fmap Param x

liftP0 :: a -> Param a
liftP0 = return

liftP :: (a -> b) -> Param a -> Param b
liftP = liftM

liftP2 :: (a -> b -> c) -> Param a -> Param b -> Param c
liftP2 = liftM2

liftP3 :: (a -> b -> c -> d) -> Param a -> Param b -> Param c -> Param d
liftP3 = liftM3

liftP4 :: (a -> b -> c -> d -> e) -> Param a -> Param b -> Param c -> Param d -> Param e
liftP4 = liftM4

liftP5 :: (a -> b -> c -> d -> e -> f) -> Param a -> Param b -> Param c -> Param d -> Param e -> Param f
liftP5 = liftM5

-- template_fmap :: (Monad m, Traversable f) => m ((a -> m b) -> m (f a -> m (f b)))
-- template_fmap = return $ return . sequence . fmap
-- template_pure :: (Monad m, Applicative f) => m (a -> m (f a))
-- template_pure = return $ return . pure
-- template_symb_oangle_symb_star_symb_cangle_ :: (Monad m, Applicative f, Traversable f) => m (f (a -> m b) -> m (f a -> m (f b)))
-- template_symb_oangle_symb_star_symb_cangle_ = return $ \f -> return $ \x -> sequence $ f <*> x
-- template_symb_cangle_symb_cangle_symb_equal_ :: (Monad m, Monad f) => m (f a -> m ((a -> m (f b)) -> m (f b)))
-- template_symb_cangle_symb_cangle_symb_equal_ = return $ \x -> return $ \f -> x >>= f

-- template_fmap :: (Monad m, Functor f) => m ((a -> b) -> m (f a -> m (f b)))
-- template_fmap = return $ \f -> return $ \x -> return $ fmap f x
-- template_pure :: (Monad m, Applicative f) => m (a -> m (f a))
-- template_pure = return $ \x -> return $ pure x
-- template_symb_oangle_symb_star_symb_cangle_ :: (Monad m, Applicative f) => m (f (a -> b) -> m (f a -> m (f b)))
-- template_symb_oangle_symb_star_symb_cangle_ = return $ \f -> return $ \x -> return $ f <*> x
-- template_symb_cangle_symb_cangle_symb_equal_ :: (Monad m, Monad f) => m (f a -> m ((a -> f b) -> m (f b)))
-- template_symb_oangle_symb_star_symb_cangle_ = return $ \x -> return $ \f -> return $ x >>= f

-- ----------------------------------------------------------------------
-- * List operations

-- | Lifted version of @'(:)' :: a -> [a] -> [a]@.
template_symb_colon_ :: Monad m => m (a -> m ([a] -> m [a]))
template_symb_colon_ = return $ \h -> return $ \t -> return (h:t)

-- | Lifted version of @'[]' :: [a]@.
template_symb_obracket_symb_cbracket_ :: Monad m => m [a]
template_symb_obracket_symb_cbracket_ = return []

-- | Lifted version of @'init' :: [a] -> [a]@.
template_init ::  Monad m => m ([a] -> m [a])
template_init = return $ \l -> return (init l)

-- | Lifted version of @'last' :: [a] -> [a]@.
template_last :: Monad m => m ([a] -> m a)
template_last = return $ \l -> return (last l)

-- | Lifted version of @'(++)' :: [a] -> [a] -> [a]@.
template_symb_plus_symb_plus_ :: Monad m => m ([a] -> m ([a] -> m [a]))
template_symb_plus_symb_plus_ = return $ \l1 -> return $ \l2-> return (l1 ++ l2)

-- | Lifted version of 'zip3'.
template_zip3 :: Monad m => m ([a] -> m ([b] -> m ([c] -> m [(a,b,c)])))
template_zip3 = return $ \x -> return $ \y -> return $ \z -> return (zip3 x y z)

-- | lifted version of @'foldl'@
template_foldl :: Monad m => m ((a -> m (b -> m a)) -> m (a -> m ([b] -> m a)))
template_foldl = return $ \f -> return $ \a -> return $ \lb -> foldM (auxf f) a lb
        where auxf f a b = do
                g <- f a
                g b

-- | lifted version of @'reverse'@
template_reverse :: Monad m => m ([a] -> m [a])
template_reverse = return $ \x -> return (reverse x)


-- | lifted version of @'zipWith'@
template_zipWith :: Monad m => m ((a -> m (b -> m c)) -> m ([a] -> m ([b] -> m [c])))
template_zipWith = return $ \f -> return $ \a -> return $ \b -> zipWithM (auxf f) a b
        where auxf f a b = do
                g <- f a
                g b

-- | Lifted version of @'fold_right_zip'@
template_fold_right_zip :: 
  Monad m => m (((a,b,c) -> m (a,d)) -> m ((a,[b],[c]) -> m (a,[d])))
template_fold_right_zip = return $ \f -> return $ \x -> (fold_right_zipM f x)

-- ----------------------------------------------------------------------
-- * Other operations

-- | Lifted version of the combinator '$'.
template_symb_dollar_ :: Monad m => m ((a -> m b) -> m (a -> m b))
template_symb_dollar_ = return $ \f -> return $ \x -> f x

-- | Lifted version of @'error' :: String -> a@. Using it will make the
-- circuit generation fail with the error described in 'String'.
template_error :: Monad m => m (String -> m a)
template_error = return $ error

-- | Lifted version of @'snd' :: (a,b) -> b@
template_snd :: Monad m => m ((a,b) -> m b)
template_snd = return $ \(a,b) -> return b



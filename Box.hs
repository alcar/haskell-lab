module Box where

import Control.Applicative
import Data.Monoid

data Box a = Cons a (Box a) | Nil deriving Eq

instance (Show a) => Show (Box a) where
    show = boxShow

instance Functor Box where
    fmap = boxMap

instance Applicative Box where
    pure        x       = Cons x Nil
    Nil         <*> _   = Nil
    (Cons f fs) <*> x   = boxAppend (fmap f x) (fs <*> x)

instance Monoid (Box a) where
    mempty  = Nil
    mappend = boxAppend

instance Monad Box where
    return          x       = Cons x Nil
    Nil             >>= _   = Nil
    (Cons x Nil)    >>= f   = f x
    fail            _       = Nil

boxAny :: (a -> Bool) -> Box a -> Bool
boxAny _ Nil            = False
boxAny t (Cons x xs)    = t x || boxAny t xs

boxAppend :: Box a -> Box a -> Box a
boxAppend x     Nil         = x
boxAppend Nil   y           = y
boxAppend x     (Cons y ys) = boxAppend (boxInsert x y) ys

boxFilter :: (a -> Bool) -> Box a -> Box a
boxFilter _ Nil         = Nil
boxFilter t (Cons x xs) = if t x then include else exclude where
    include = Cons x $ boxFilter t xs
    exclude = boxFilter t xs

boxFilter' :: (a -> Bool) -> Box a -> Box a
boxFilter' t = boxFoldr (\ x xs -> if t x then Cons x xs else xs) Nil

boxFoldr :: (a -> b -> b) -> b -> Box a -> b
boxFoldr _ acc Nil          = acc
boxFoldr f acc (Cons x xs)  = f x $ boxFoldr f acc xs

boxIndexes :: Num b => Box a -> Box b
boxIndexes box = boxFoldr (\ x c s -> Cons s (c (s + 1))) (const Nil) box 0

boxInsert :: Box a -> a -> Box a
boxInsert Nil           t = Cons t Nil
boxInsert (Cons x xs)   t = Cons x (boxInsert xs t)

boxLength :: Num b => Box a -> b
boxLength Nil           = 0
boxLength (Cons x xs)   = 1 + boxLength xs

boxMap :: (a -> b) -> Box a -> Box b
boxMap _ Nil            = Nil
boxMap f (Cons x xs)    = Cons (f x) (boxMap f xs)

boxMap' :: (a -> b) -> Box a -> Box b
boxMap' f = boxFoldr (\ x y -> Cons (f x) y) Nil

boxScanl :: (a -> b -> b) -> b -> Box a -> Box b
boxScanl f k box = Cons k $ boxFoldr cps (const Nil) box k
    where cps x c s = Cons (f x s) (c (f x s))

boxScanr :: (a -> b -> b) -> b -> Box a -> Box b
boxScanr _ k Nil            = Cons k        Nil 
boxScanr f k (Cons x xs)    = Cons calcTerm $ boxScanr f k xs
    where calcTerm = boxFoldr f k (Cons x xs)

boxShow :: Show a => Box a -> String
boxShow Nil         = "[]"
boxShow (Cons x xs) = "[" ++ show x ++ showRest xs ++ "]" where
    showRest Nil            = ""
    showRest (Cons x xs)    = "," ++ show x ++ showRest xs

boxSum :: Box Integer -> Integer
boxSum = boxFoldr (+) 0

boxZip :: Box a -> Box a -> Box (a, a)
boxZip _            Nil         = Nil
boxZip (Cons x xs)  (Cons y ys) = Cons (x, y) $ boxZip xs ys
boxZip x            y           = flip boxZip x y

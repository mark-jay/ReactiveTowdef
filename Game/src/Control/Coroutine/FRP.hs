{-# LANGUAGE BangPatterns #-}

module Control.Coroutine.FRP where

import qualified Control.Category as C
import Control.Arrow
import Control.Applicative (liftA2)
import Data.Monoid
import Data.List (foldl')

import Control.Coroutine
import qualified Data.IntMap as IntMap

type Event a = [a]

edge :: Eq a => Coroutine a (Event a)
edge = Coroutine $ \i -> ([i], Just $ step i) where
    step old = Coroutine $ \i ->
        if old == i
            then ([],  Just $ step i)
            else ([i], Just $ step i)

watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], Just $ watch f)
        else ([], Just $ watch f)

withPrevious :: a -> Coroutine a (a,a)
withPrevious first = Coroutine $ \i -> ((i, first), Just $ step i) where
    step old = Coroutine $ \i -> ((i, old), Just $ step i)

withPrevious' :: Coroutine a (a,a)
withPrevious' = Coroutine $ \i -> ((i,i), Just $ step i) where
    step old = Coroutine $ \i -> ((i, old), Just $ step i)

delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl' f a e in (a', Just $ scanE f a')

mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

concatMapE :: (e -> Event e') -> Coroutine (Event e) (Event e')
concatMapE = arr . concatMap

filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)

zipE' :: Coroutine (Event e, Event e') (Event (e, e'))
zipE' = arr $ uncurry zip

mergeE :: Coroutine i (Event e) -> Coroutine i (Event e) -> Coroutine i (Event e)
mergeE = (<++>)

(<++>) :: Monoid o => Coroutine i o -> Coroutine i o -> Coroutine i o
(<++>) = liftA2 mappend

constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

stepE :: a -> Coroutine (Event a) a
stepE a = Coroutine $ \ev ->
    let a' = last (a:ev)
    in (a', Just $ stepE a')

restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine $ step co where
    step c (i, ev) = (o, fmap Coroutine cont) where
        (o, c') = runC c i
        cont
            | null ev   = fmap step c'
            | otherwise = Just $ step co

delayE :: Int -> Coroutine (Event e) (Event e)
delayE delay = arr (const delay) &&& C.id >>> delayEn

delayEn :: Coroutine (Int, Event e) (Event e)
delayEn = Coroutine $ step 0 IntMap.empty where
    step !cur !buffer (delay, ev) = (ev', Just $ Coroutine $ step (cur+1) buffer') where
        ev' = IntMap.findWithDefault [] cur buffer'
        buffer'
            | null ev   = buffer
            | otherwise = IntMap.insertWith (++) (cur+delay) ev buffer

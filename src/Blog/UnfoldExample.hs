{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE KindSignatures   #-}

module Blog.UnfoldExample (
    getData
  , process
  , processPacket
  , run
  , Cursor(..)
  , CursorState(..)
  , Packet(..)
) where

import Data.List (intercalate)
import Control.Monad.Loops (unfoldrM)
import Data.Kind (Type)

newtype Cursor = Cursor Int deriving Eq
data CursorState = NewCursor | GoCursor Cursor | StopCursor

data Packet = Packet { value :: String, cursor :: Maybe Cursor }

getData :: forall (m :: Type -> Type). Applicative m => Maybe Cursor -> m Packet
getData Nothing = pure $ Packet "packet one" (Just $ Cursor 1)
getData (Just (Cursor cur))
  | cur == 1  = pure $ Packet "packet two"   (Just $ Cursor 2)
  | cur == 2  = pure $ Packet "packet three" (Just $ Cursor 3)
  | cur == 3  = pure $ Packet "packet four"  (Just $ Cursor 4)
  | otherwise = pure $ Packet "packet five"  Nothing

process :: forall (m :: Type -> Type). Applicative m => (Maybe Cursor -> m Packet) -> CursorState -> m (Maybe (String, CursorState))
process f NewCursor = 
  let resultM :: m Packet = f Nothing
  in  Just . processPacket <$> resultM
process f (GoCursor c) = 
  let resultM :: m Packet = f (Just c)
  in  Just . processPacket <$> resultM
process _ StopCursor = pure Nothing 


processPacket :: Packet -> (String, CursorState)
processPacket (Packet v (Just c)) = (v, GoCursor c)
processPacket (Packet v Nothing)  = (v, StopCursor)
-- unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]

run :: IO ()
run = 
  let resultsIO :: IO [String] = unfoldrM (process getData) NewCursor
      stringyfied :: IO String = (intercalate "," . take 3) <$> resultsIO
  in stringyfied >>= putStrLn

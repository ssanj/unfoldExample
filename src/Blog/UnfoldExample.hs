{-# LANGUAGE ScopedTypeVariables   #-}

module Blog.UnfoldExample (
    serviceCall
  , extractState
  , Packet(..)
) where

import Blog.UnfoldWith (Cursor(..), CursorState(..))

data Packet = Packet { value :: String, cursor :: Maybe (Cursor Int) }

serviceCall :: forall m . Applicative m => Maybe (Cursor Int) -> m Packet
serviceCall Nothing = pure $ Packet "packet one" (Just $ Cursor 1)
serviceCall (Just (Cursor cur))
  | cur == 1  = pure $ Packet "packet two"   (Just $ Cursor 2)
  | cur == 2  = pure $ Packet "packet three" (Just $ Cursor 3)
  | cur == 3  = pure $ Packet "packet four"  (Just $ Cursor 4)
  | otherwise = pure $ Packet "packet five"  Nothing

extractState :: Packet -> (String, CursorState Int)
extractState (Packet v (Just c)) = (v, GoCursor c)
extractState (Packet v Nothing)  = (v, StopCursor)

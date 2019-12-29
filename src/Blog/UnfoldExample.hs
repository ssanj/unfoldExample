{-# LANGUAGE ScopedTypeVariables   #-}

module Blog.UnfoldExample (
    serviceCall
  , unfoldWith
  , extractState
  , run
  , Cursor(..)
  , CursorState(..)
  , Packet(..)
) where

import Data.List (intercalate)
import Control.Monad.Loops (unfoldrM)
import Data.Kind (Type)

-- | Wrapper around a cursor of some sort
newtype Cursor a = Cursor a deriving Eq

-- | An ADT to capture the three states of a cursor:
-- NewCursor - Initial cursor state
-- GoCursor - A state of having a cursor, typically used for iteration
-- StopCursor - The final cursor state
data CursorState a = NewCursor | GoCursor (Cursor a) | StopCursor

-- | A Generic function that unfolds a computation that has some form of 'Cursor'; such as a paginated REST API.
unfoldWith :: forall m a b c. Applicative m => 
  (a -> (b, CursorState c)) -> 
  -- ^ a function that can extract a "result" and the next 'CursorState' from some response 'a'
  (Maybe (Cursor c) -> m a)   -> 
  -- ^ a function that takes an optional 'Cursor' and returns a response 'a' in the effect 'm'
  (CursorState c)            -> 
  -- ^ an initial 'CursorState'
  m (Maybe (b, CursorState c))
  -- ^ the optional pair of the result type 'b' with the next 'CursorState' in the effect 'm'
unfoldWith extractPayload iterateWith NewCursor = 
  let resultM :: m a = iterateWith Nothing
  in  Just . extractPayload <$> resultM
unfoldWith extractPayload iterateWith (GoCursor c) = 
  let resultM :: m a = iterateWith (Just c)
  in  Just . extractPayload <$> resultM
unfoldWith _ _ StopCursor = pure Nothing 

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
-- unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]

run :: IO ()
run = 
  let resultsIO :: IO [String] = unfoldrM (unfoldWith extractState serviceCall) NewCursor
      stringyfied :: IO String = (intercalate "," . take 3) <$> resultsIO
  in stringyfied >>= putStrLn

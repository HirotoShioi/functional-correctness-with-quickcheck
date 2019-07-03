{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Solution.Tries
  ( Trie ()
  , empty
  , null
  , valid
  , insert
  , lookup
  , delete
  , toList
  , keys
  , elems
  , member
  , union
  , size
  ) where

import           Control.Monad.State.Strict (State, execState, forM_, modify')
import           Data.List                  (foldl')
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)
import           Prelude                    hiding (lookup, null)
import           Test.QuickCheck            (Arbitrary (..), Gen)

-- |A @'Trie' a b@ is a map with keys of type @[a]@ and values of type @b@.
data Trie a b = Fork (Maybe b) (Map a (Trie a b))
  deriving (Show, Eq)

instance Functor (Trie a) where
  fmap :: (b -> c) -> Trie a b -> Trie a c
  fmap f (Fork mb m) = Fork (f <$> mb) (fmap f <$> m)

instance Foldable (Trie a) where

  foldr :: (b -> r -> r) -> r -> Trie a b -> r
  foldr f c t = foldr f c $ trieToList t
   where
    trieToList (Fork mb m) =
      let xs = concatMap trieToList m
      in  case mb of
        Nothing -> xs
        Just b  -> b : xs

instance (Ord a) => Semigroup (Trie a b) where
  (<>) = union

instance (Ord a) => Monoid (Trie a b) where
  mempty = empty
  mappend = union

-- |The empty trie.
--
-- >>> length empty
-- 0
empty :: Trie a b
empty = Fork Nothing M.empty

-- |Checks whether a trie is empty.
-- >>> null empty
-- True
-- >>> null (insert "IOHK" True empty)
-- False
null :: Trie a b -> Bool
null (Fork (Just _) _) = False
null (Fork Nothing m)  = all null m

-- |Checks whether a trie is valid
valid :: Trie a b -> Bool
valid (Fork _ m) = all (\t -> valid t && not (null t)) m

-- |Inserts a value for a key into a trie.
insert :: Ord a
       => [a]
       -> b
       -> Trie a b
       -> Trie a b
insert []       b (Fork _ m)  = Fork (Just b) m
insert (a : as) b (Fork mb m) =
  let m' = case M.lookup a m of
         Nothing -> M.insert a (insert as b empty) m
         Just _  -> M.adjust (insert as b) a m
  in  Fork mb m'

-- |Looks up a key in a trie. Returns @'Just'@ the value or @'Nothing'@.
--
-- >>> lookup "foo" empty
-- Nothing
-- >>> lookup "foo" (insert "foo" 42 empty)
-- Just 42
lookup :: Ord a
       => [a]
       -> Trie a b
       -> Maybe b
lookup []       (Fork mb _) = mb
lookup (a : as) (Fork _  m) = do
  t <- M.lookup a m
  lookup as t

-- |Deletes a key (and its associated value) from a trie.
--
-- >>> lookup "foo" $ delete "foo" $ insert "foo" 42 empty
-- Nothing
delete :: Ord a
       => [a]
       -> Trie a b
       -> Trie a b
delete [] (Fork _ m)          = Fork Nothing m
delete (a : as) t@(Fork mb m) = case M.lookup a m of
  Nothing -> t -- Couldn't find key, so don't delete anything
  Just t' ->   -- We found the key
    let m' = if null (delete as t')     -- Check if previous recursion made null node
          then M.delete a m             -- If so, delete it
          else M.adjust (delete as) a m -- If not, keep going
    in  Fork mb m'

instance (Arbitrary key, Arbitrary value, Ord key) => Arbitrary (Trie key value) where
  arbitrary = do
    kvs <- (arbitrary :: Gen [([key],value)])
    return $ foldl' (\acc (k,v) -> insert k v acc) empty kvs

-- | Converts a trie to a Map of key-value pairs.
toList :: forall a b. (Ord a) => Trie a b -> Map [a] b
toList t = execState (collectKeyValue [] t) mempty
  where
    collectKeyValue :: [a] -> Trie a b -> State (Map [a] b) ()
    collectKeyValue curr (Fork mVal m) = do
      whenJust mVal $ \val -> modify' (M.insert curr val)
      let kvs = M.toList m
      forM_ kvs $ \(c, t) -> collectKeyValue (curr <> [c]) t
    whenJust :: (Monad m) => Maybe c -> (c -> m ()) -> m ()
    whenJust (Just a) f = f a
    whenJust Nothing _  = return ()

-- | Returns the keys stored in a trie.
keys :: (Ord a) => Trie a b -> [[a]]
keys = M.keys . toList

-- | Returns the values stored in a trie.
-- [17,42]
elems :: (Ord a) => Trie a b -> [b]
elems = M.elems . toList

-- | Checks whether a given key is stored in a trie.
--
-- >>> member "foo" empty
-- False
-- >>> member "foo" $ insert "foo" 42 empty
-- True
member :: Ord a => [a] -> Trie a b -> Bool
member key t = isJust $ lookup key t

union :: Ord a => Trie a b -> Trie a b -> Trie a b
union t1 t2 = M.foldlWithKey' (\acc key value -> insert key value acc) t2 (toList t1)

-- Trie is foldable!
size :: Ord a => Trie a b -> Int
size = length 
{-| This module introduces some of the test cases you can implement for the Trie
-}
{-# LANGUAGE TemplateHaskell #-}

module Solution.TrieSpec where

import           Data.List       (nubBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prelude         hiding (lookup, null)
import           Solution.Tries  (Trie (..), delete, empty, insert, lookup,
                                  member, size, toList, union, valid)
import           Test.QuickCheck (Arbitrary (..), Property, conjoin,
                                  counterexample, elements, label,
                                  quickCheckAll, (.&&.), (===), (==>))

type TestTrie = Trie KeySource Value
type KeyValue = (Key, Value)
type Key = [KeySource]
type Value = Int

-- | Used as source of an key
-- When testing trie, you want your tries to have certain number of common prefixes.
-- Limiting the number of keys would allow the generator to generate trie with
-- commmon prefixes reliably.
data KeySource
  = A
  | B
  | C
  | D
  deriving (Show, Eq, Ord, Enum)

instance Arbitrary KeySource where
    arbitrary = elements [A .. D]

--------------------------------------------------------------------------------
-- Validity
--------------------------------------------------------------------------------

prop_EmptyValid :: Bool
prop_EmptyValid = valid empty

-- We test that all generated tries are valid
prop_GenValid :: TestTrie -> Bool
prop_GenValid = valid

prop_InsertValid :: KeyValue -> TestTrie -> Bool
prop_InsertValid (k, v) t = valid $ insert k v t

prop_DeleteValid :: Key -> TestTrie -> Bool
prop_DeleteValid k t = valid $ delete k t

prop_UnionValid :: TestTrie -> TestTrie -> Bool
prop_UnionValid t1 t2 = valid $ t1 `union` t2

--------------------------------------------------------------------------------
-- Post condition
--------------------------------------------------------------------------------

prop_Insert :: KeyValue -> TestTrie -> Property
prop_Insert (k, v) t = lookup k (insert k v t) === Just v

prop_Delete :: KeyValue -> TestTrie -> Property
prop_Delete (k, v) t = lookup k (delete k $ insert k v t) === Nothing

prop_Size :: [KeyValue] -> Property
prop_Size xs = xs == nubBy (\(k1, _) (k2, _) -> k1 == k2) xs ==>
    let t' = foldl (\acc (k,v) -> insert k v acc) empty xs
    in size t' === length xs

-- We test that our abstraction function preserves the content of the trie
-- We've proved that the size function work properly in the previous test so we
-- can use it as a proof!
prop_ToList :: TestTrie -> Property
prop_ToList t =
    let keyValueList = M.toList $ toList t
        props = map
            (\(k, v) -> lookup k t === Just v)
            keyValueList
    in conjoin props .&&. size t === M.size (toList t)

--------------------------------------------------------------------------------
-- Metamorphic testing
--------------------------------------------------------------------------------

prop_InsertInsert :: KeyValue -> KeyValue -> TestTrie -> Property
prop_InsertInsert (k1, v1) (k2, v2) t =
    insert k2 v2 (insert k1 v1 t)
    ===
    if k2 == k1
      then insert k2 v2 t
      else insert k2 v2 (insert k1 v1 t)

prop_InsertDelete :: KeyValue -> TestTrie -> Property
prop_InsertDelete (k, v) t =
    delete k (insert k v t)
    ===
    if member k t
        then delete k t
        else t

-- I feel like this is a very weak property..
prop_DeleteSize :: Key -> TestTrie -> Bool
prop_DeleteSize k t = size (delete k t) <= size t

prop_UnionElem :: TestTrie -> TestTrie -> Property
prop_UnionElem t1 t2 =
    let unions  = t1 `union` t2
        keyValueList = M.toList $ toList t1
        props = map
            -- This way, we ensure that union preserves the content of the left trie
            (\(key, value) -> lookup key unions === Just value)
            keyValueList
    in conjoin props

-- | Try to predict the size of the trie when you apply multiple inserts
-- into existing tree
prop_SizeMeta :: [KeyValue] -> TestTrie -> Property
prop_SizeMeta xs t =
    let (added, t') = computeSize xs t 0
    in added + size t === size t'
  where
    computeSize :: [KeyValue] -> TestTrie -> Int -> (Int, TestTrie)
    computeSize [] t num          = (num, t)
    computeSize ((k, v):xs) t num =
        let num' = if member k t
               then num
               else num + 1
        in computeSize xs (insert k v t) num'

--------------------------------------------------------------------------------
-- Model based testing
--------------------------------------------------------------------------------

prop_InsertModel :: KeyValue -> TestTrie -> Property
prop_InsertModel (k, v) t = insert k v t =~= M.insert k v (toList t)

-- On this case, we're extensively debugging the test code using 'label' and 'counterExample'
-- What I've found is that, 99% of the times, generated trie does not have key-value
-- pair that we're trying to delete. This means delete is nothing at all, making
-- hard for QuickCheck to find bugs.
prop_DeleteModel :: KeyValue -> TestTrie -> Property
prop_DeleteModel (k, v) t = label haskey $
    delete k (insert k v t)
    =~=
    M.delete k (toList $ insert k v t)
  where
    haskey = if member k t
        then "Generated trie already has key value pair inserted"
        else "Generated trie does not have key-value pair"

prop_UnionModel :: TestTrie -> TestTrie -> Property
prop_UnionModel t1 t2 =
    (t1 <> t2) =~= (toList t1 <> toList t2)

(=~=) :: TestTrie -> Map Key Value -> Property
t =~= m = 
    counterexample ("Model: " <> showCounterExample t) $
    toList t === m
  where
    showCounterExample :: TestTrie -> String
    showCounterExample = show . toList

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

prop_MonoidLaw :: TestTrie -> TestTrie -> TestTrie -> Property
prop_MonoidLaw t1 t2 t3 =
         t1 === t1 <> mempty
    .&&. t1 <> (t2 <> t3) === (t1 <> t2) <> t3

prop_FunctorLaw :: TestTrie -> Property
prop_FunctorLaw t =
         fmap id t === t
    .&&. fmap (show . (+10)) t === (fmap show . fmap (+10) $ t)

return []
runTests :: IO Bool
runTests = $quickCheckAll

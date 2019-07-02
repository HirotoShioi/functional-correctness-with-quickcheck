{-# LANGUAGE TemplateHaskell #-}

module Examples.BSTSpec where

import qualified Data.List       as L
import           Test.QuickCheck

import           Examples.BST

--------------------------------------------------------------------------------
-- Validty
--------------------------------------------------------------------------------

prop_ArbitraryValid :: BST Int Int -> Bool
prop_ArbitraryValid = valid

prop_InsertValid :: Int -> Int -> BST Int Int -> Bool
prop_InsertValid k v t = valid (insert k v t)

--------------------------------------------------------------------------------
-- postcondition properties
--------------------------------------------------------------------------------

prop_InsertPost :: Int -> Int -> BST Int Int -> Int -> Property
prop_InsertPost k v t k' =
  find k' (insert k v t)
  ===
  if k==k' then Just v else find k' t

prop_FindPostPresent :: Int -> Int -> BST Int Int -> Property
prop_FindPostPresent k v t =
  find k (insert k v t) === Just v

prop_FindPostAbsent :: Int -> BST Int Int -> Property
prop_FindPostAbsent k t =
  find k (delete k t) === Nothing

--------------------------------------------------------------------------------
-- metamorphic properties
--------------------------------------------------------------------------------

prop_SizeInsert :: Int -> Int -> BST Int Int -> Bool
prop_SizeInsert k v t =
  size (insert k v t) >= size t

(=~=) :: (Eq k, Eq v, Show k, Show v) => BST k v -> BST k v -> Property
t1 =~= t2 =
  toList t1 === toList t2

prop_InsertInsert :: Int -> Int -> Int -> Int -> BST Int Int -> Property
prop_InsertInsert k v k' v' t =
  insert k v (insert k' v' t)
  =~=
  if k==k' then insert k v t else insert k' v' (insert k v t)

--------------------------------------------------------------------------------
-- Model based properties
--------------------------------------------------------------------------------

prop_InsertModel :: Int -> Int -> BST Int Int -> Property
prop_InsertModel k v t =
  toList (insert k v t) === L.insert (k,v) (deleteKey k $ toList t)

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/=k) . fst)

-- Test all properties in the module: don't touch this code!

return []
runTests :: IO Bool
runTests = $quickCheckAll

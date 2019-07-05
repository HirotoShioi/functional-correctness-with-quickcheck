{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.BSTSpec where

import           Control.Applicative ((<|>))
import qualified Data.List           as L
import           Data.Maybe          (isJust)
import           GHC.Generics        (Generic)
import           Test.QuickCheck     (Arbitrary (..), Property, label,
                                      quickCheckAll, shuffle, (===))

import           Examples.BST

type Key = Int
type Val = Int
type Tree = BST Int Int

--------------------------------------------------------------------------------
-- Validty
--------------------------------------------------------------------------------

prop_ArbitraryValid :: Tree -> Bool
prop_ArbitraryValid = valid

-- It's reaaally slow
-- prop_ShrinkValid :: Tree -> Bool
-- prop_ShrinkValid t = all valid (shrink t)

prop_InsertValid :: Key -> Val -> Tree -> Bool
prop_InsertValid k v t = valid (insert k v t)

prop_DeleteValid :: Key -> Tree -> Bool
prop_DeleteValid k t = valid (delete k t)

prop_UnionValid :: Tree -> Tree -> Bool
prop_UnionValid t1 t2 = valid (t1 `union` t2)

--------------------------------------------------------------------------------
-- post-condition
--------------------------------------------------------------------------------

prop_InsertPost :: Int -> Int -> Tree -> Int -> Property
prop_InsertPost k v t k' =
  find k' (insert k v t)
  ===
  if k==k' then Just v else find k' t

prop_InsertPostSameKey :: Key -> Val -> Tree -> Property
prop_InsertPostSameKey k v t = prop_InsertPost k v t k

prop_UnionPost :: Tree -> Tree -> Key -> Property
prop_UnionPost t1 t2 k = find k (t1 `union` t2) === (find k t1 <|> find k t2)

prop_FindPostPresent :: Int -> Int -> Tree -> Property
prop_FindPostPresent k v t =
  find k (insert k v t) === Just v

prop_FindPostAbsent :: Int -> Tree -> Property
prop_FindPostAbsent k t =
  find k (delete k t) === Nothing

prop_InsertDeleteComplete :: Key -> Tree -> Property
prop_InsertDeleteComplete k t = case find k t of
  Nothing -> t === delete k t
  Just v  -> t === insert k v t

--------------------------------------------------------------------------------
-- metamorphic properties
--------------------------------------------------------------------------------

prop_SizeInsert :: Int -> Int -> Tree -> Bool
prop_SizeInsert k v t =
  size (insert k v t) >= size t

(=~=) :: (Eq k, Eq v, Show k, Show v) => BST k v -> BST k v -> Property
t1 =~= t2 =
  toList t1 === toList t2

prop_InsertInsert :: Int -> Int -> Int -> Int -> Tree -> Property
prop_InsertInsert k v k' v' t =
  insert k v (insert k' v' t)
  =~=
  if k==k'
    then insert k v t
    else insert k' v' (insert k v t)

-- This test will not work!
-- prop_InsertPreserveEquiv :: Key -> Val -> Tree -> Tree -> Property
-- prop_InsertPreserveEquiv k v t t' = toList t == toList t' ==>
--   insert k v t =~= insert k v t'

data Equivs k v = Equivs (BST k v) (BST k v)
  deriving (Show, Generic)

instance (Arbitrary k, Arbitrary v, Ord k, Eq v) => Arbitrary (Equivs k v) where
  arbitrary = do
    kvs  <- L.nubBy (\(k1, _) (k2, _) -> k1 == k2) <$> arbitrary
    kvs' <- shuffle kvs
    return $ Equivs (tree kvs) (tree kvs')
    where
      tree = foldr (uncurry insert) nil
  shrink (Equivs t t') = L.foldl'
    (\acc (tree, tree') -> if toList tree == toList tree'
      then acc <> [Equivs tree tree']
      else acc
    )
    mempty
    (zip (shrink t) (shrink t'))

type EquivTrees = Equivs Key Val

--------------------------------------------------------------------------------
-- Inductive testing
--------------------------------------------------------------------------------

-- It's important that we test this first
prop_Equivs :: EquivTrees -> Property
prop_Equivs (Equivs t t') = t =~= t'

-- It's reaaally slow
-- prop_ShrinkEquivs :: EquivTrees -> Property
-- prop_ShrinkEquivs equivs = conjoin $ map (\(Equivs t t') -> t =~= t') (shrink equivs)

prop_UnionNil :: Tree -> Property
prop_UnionNil t = union nil t === t

prop_InsertPreserveEquiv :: Key -> Val -> Equivs Int Int -> Property
prop_InsertPreserveEquiv k v (Equivs t t') = insert k v t =~= insert k v t'

prop_DeletePreservesEquiv :: Key -> EquivTrees -> Property
prop_DeletePreservesEquiv k (Equivs t t') = delete k t =~= delete k t'

prop_UnionPreservesEquiv :: EquivTrees -> EquivTrees -> Property
prop_UnionPreservesEquiv (Equivs t1 t1') (Equivs t2 t2') = (t1 `union` t2) =~= (t1' `union` t2')

prop_FindPreservesEquiv :: Key -> EquivTrees -> Property
prop_FindPreservesEquiv k (Equivs t t') = find k t === find k t'

prop_InsertComplete :: Tree -> Property
prop_InsertComplete t = t === L.foldl (flip $ uncurry insert) nil (insertions t)

prop_InsertCompleteForDelete :: Key -> Tree -> Property
prop_InsertCompleteForDelete k t = prop_InsertComplete (delete k t)

prop_InsertCompleteForUnion :: Tree -> Tree -> Property
prop_InsertCompleteForUnion t t' = prop_InsertComplete (t `union` t')

--------------------------------------------------------------------------------
-- Model based properties
--------------------------------------------------------------------------------

prop_NilModel :: Property
prop_NilModel = toList (nil :: Tree) === []

prop_InsertModel :: Int -> Int -> Tree -> Property
prop_InsertModel k v t =
  toList (insert k v t) === L.insert (k,v) (deleteKey k $ toList t)

prop_DeleteModel :: Key -> Tree -> Property
prop_DeleteModel k t = toList (delete k t) === deleteKey k (toList t)

prop_UnionModel :: Tree -> Tree -> Property
prop_UnionModel t t' = toList (t `union` t') ===
  L.sort ((L.unionBy (\a a' -> fst a == fst a')) (toList t) (toList t'))

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/=k) . fst)

--------------------------------------------------------------------------------
-- Measurement
--------------------------------------------------------------------------------

prop_Measure :: Key -> Tree -> Property
prop_Measure k t =
    label hasKey
  $ label (keyPos k keys)
  True -- Make it so that the test always passes
  where
    hasKey = if isJust (find k t)
      then "present"
      else "absent"

    keyPos k keys
      | null keys       = "empty"
      | [k] == keys     = "just"
      | all (<= k) keys = "at start"
      | all (>= k) keys = "at end"
      | otherwise       = "middle"
    keys = map fst $ toList t

-- Test all properties in the module: don't touch this code!

return []
runTests :: IO Bool
runTests = $quickCheckAll

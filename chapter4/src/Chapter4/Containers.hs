{-# LANGUAGE FlexibleContexts #-}

module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import Chapter3.ParamPoly

{-
 - *Chapter4.Containers> M.insert 2 "dos" . insert' 1 "uno" $ M.fromList []
 - fromList [(1,"uno"),(2,"dos")]
 -}
insert' :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insert' k a m = M.alter (\_ -> Just a) k m

delete' :: (Ord k) => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (\_ -> Nothing) k m

{-
 - *Chapter4.Containers> adjust' (\x -> x ++ " plus one") 2 $ M.fromList [(1, "uno"), (2, "dos")]
 - fromList [(1,"uno"),(2,"dos plus one")]
 -}
adjust' :: (Ord k) => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' f k m = M.alter repl k m
                where repl = (\x -> case x of
                              Just a -> Just $ f a
                              _ -> Nothing
                             )


data ClientKind = GovOrgKind | CompanyKind | IndividualKind
                  deriving (Ord, Eq, Show)

classifyClients :: (Ord (Client Integer)) => [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients [] = M.empty
classifyClients (x:xs) = let kind = case x of
                                    GovOrg _ _      -> GovOrgKind
                                    Company _ _ _ _ -> CompanyKind
                                    Individual _ _  -> IndividualKind
                             append set = case set of
                                          Nothing -> Just $ S.singleton x
                                          Just s  -> Just $ S.insert x s
                         in M.alter append kind $ classifyClients xs

classifyClients' :: (Ord (Client Integer)) => [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' xs = M.fromList [ (GovOrgKind, S.fromList govOrgs)
                                 , (CompanyKind, S.fromList companies)
                                 , (IndividualKind, S.fromList individuals) ]
                      where govOrgs = filter (\x -> case x of
                                                    GovOrg _ _ -> True
                                                    _          -> False
                                             ) xs
                            companies = filter (\x -> case x of
                                                      Company _ _ _ _ -> True
                                                      _               -> False
                                               ) xs
                            individuals = filter (\x -> case x of
                                                        Individual _ _ -> True
                                                        _              -> False
                                                 ) xs



data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2
                     deriving (Show)

{- doesn't work without Ord v -}
{-instance Functor BinaryTree2 where
    fmap f (Node2 v l r) = treeInsert2 (f v) $ fmap f l `joinTrees2` fmap f r
    fmap _ Leaf2 = Leaf2-}

treeFind2 :: (Ord a) => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treeFind2 t l
                            GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing

treeInsert2 :: (Ord a) => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                EQ -> n
                                LT -> Node2 v (treeInsert2 t l) r
                                GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2

joinTrees2 :: (Ord a) => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
joinTrees2 Leaf2 b = b
joinTrees2 a Leaf2 = a
joinTrees2 a (Node2 v l r) = (treeInsert2 v a) `joinTrees2` l `joinTrees2` r

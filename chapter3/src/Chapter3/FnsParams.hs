module Chapter3.FnsParams where

{- import type and all constructors: -}
import Chapter3.ParamPoly (Client(..))


filterOnes :: (Eq a, Num a) => [a] -> [a]
filterOnes xs = filter (\x -> x == 1) xs

filterANumber :: (Eq a, Num a) => a -> [a] -> [a]
filterANumber n xs = filter (== n) xs

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f xs = filter (not . f) xs

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs = filter isGovOrg

filterGovOrgs' :: [Client a] -> [Client a]
filterGovOrgs' = filter (\x -> case x of
                               GovOrg _ _ -> True
                               _          -> False
                        )

isGovOrg :: Client a -> Bool
isGovOrg (GovOrg _ _) = True
isGovOrg _ = False

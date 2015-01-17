{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.DataTypes where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown
              deriving Show

{-
 - *Chapter2.DataTypes> :t Individual (Person "Jack" "Sparrow" Male) True
 - Individual (Person "Jack" "Sparrow" Male) True :: Client
 -
 - *Chapter2.DataTypes> :t Company "Pear INC." 342 (Person "Jack" "Smith" Male) "CEO"
 - Company "Pear INC." 342 (Person "Jack" "Smith" Male) "CEO" :: Client
 -}

data TimeMachine = TimeMachine { manufacturer :: String
                               , model :: Int
                               , name :: String
                               , isInPast :: Bool
                               , isInFuture :: Bool
                               , price :: Float
                               } deriving Show

applyDiscount :: Float -> [TimeMachine] -> [TimeMachine]
applyDiscount _ [] = []
applyDiscount discount (x@(TimeMachine { price }) : xs) =
    let newPrice = price * (100 - discount) / 100
    in x { price = newPrice } : applyDiscount discount xs

{-
 - *Chapter2.DataTypes> let teslaS = TimeMachine { manufacturer = "Tesla", model = 1, name = "Model S", isInPast = False, isInFuture = True, price = 1000 }
 - *Chapter2.DataTypes> teslaS
 - TimeMachine {manufacturer = "Tesla", model = 1, name = "Model S", isInPast = False, isInFuture = True, price = 1000.0}
 - *Chapter2.DataTypes> applyDiscount 15 [teslaS]
 - [TimeMachine {manufacturer = "Tesla", model = 1, name = "Model S", isInPast = False, isInFuture = True, price = 850.0}]
 -}


clientName :: Client -> String
clientName client = case client of
                    GovOrg name -> name
                    Company name _ _ _ -> name
                    Individual (Person fName lName _) _ -> fName ++ " " ++ lName

type GenderStat = (Int, Int, Int)

genderStat :: [Client] -> GenderStat
genderStat [] = (0, 0, 0)
genderStat (client:clients) = let tailStats = genderStat clients in
                              case client of
                              Company _ _ (Person _ _ gender) _ -> countGender gender tailStats
                              Individual (Person _ _ gender) _ -> countGender gender tailStats
                              _ -> tailStats
                              where countGender gender (m, f, u) = case gender of
                                                                   Male -> (m + 1, f, u)
                                                                   Female -> (m, f + 1, u)
                                                                   Unknown -> (m, f, u + 1)


{- view pattern (require ViewPatterns extension) -}

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

{-
 - *Chapter2.DataTypes> specialClient (Company "Pear INC." 342 (Person "Jack" "Smith" Male) "Director")
 - True
 -}


data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

{-
 - *Chapter2.DataTypes> GovOrgR "NATO"
 - GovOrgR {clientRName = "NATO"}
 -
 - *Chapter2.DataTypes> IndividualR { person = PersonR { firstName = "John", lastName = "Smith" } }
 - IndividualR {person = PersonR {firstName = "John", lastName = "Smith"}}
 -}

greets :: ClientR -> String
{- NamedFieldPuns: -}
greets IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName
{- RecordWildCards: -}
greets CompanyR { .. } = "Hello, " ++ clientRName
greets GovOrgR { } = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial) : rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p

{-
 - *Chapter2.DataTypes Data.Char> nameInCapitals PersonR { firstName = "john", lastName = "smith" }
 - PersonR {firstName = "John", lastName = "smith"}
 - *Chapter2.DataTypes Data.Char> nameInCapitals PersonR { firstName = "", lastName = "smith" }
 - PersonR {firstName = "", lastName = "smith"}
 -}

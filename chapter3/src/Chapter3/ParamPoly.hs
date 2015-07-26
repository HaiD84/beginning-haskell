{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Chapter3.ParamPoly where

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person , duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq)

data Person = Person { firstName :: String , lastName :: String
                     } deriving (Show, Ord, Eq)

instance Eq i => Ord (Client i) where
    compare a b = let nameA = getClientName a
                      nameB = getClientName b
                      typeCompare
                          (Company { person = personA, duty = dutyA })
                          (Company { person = personB, duty = dutyB })
                          = let comparePersons = compare personA personB
                            in if comparePersons == EQ
                               then compare dutyA dutyB
                               else comparePersons
                      typeCompare (Individual {}) _ = LT
                      typeCompare _ (Individual {}) = GT
                      typeCompare (Company {}) (GovOrg {}) = LT
                      typeCompare (GovOrg {}) _ = GT
                  in
                  if nameA < nameB then LT
                  else if nameA > nameB then GT
                  else typeCompare a b

getClientName :: Client a -> String
getClientName (GovOrg { clientName }) = clientName
getClientName (Company { clientName }) = clientName
getClientName (Individual { person = Person {firstName, lastName} }) = firstName ++ " " ++ lastName

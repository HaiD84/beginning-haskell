{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.ParamPoly where

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person , duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String , lastName :: String
                     } deriving Show

getClientName :: Client a -> String
getClientName (GovOrg { clientName }) = clientName
getClientName (Company { clientName }) = clientName
getClientName (Individual { person = Person {firstName, lastName} }) = firstName ++ " " ++ lastName

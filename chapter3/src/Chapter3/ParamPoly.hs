module Chapter3.ParamPoly where

data Client i = GovOrg  { clientId :: i, clientRName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person , duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String , lastName :: String
                     } deriving Show

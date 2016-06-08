{-# LANGUAGE ViewPatterns #-}

module Chapter2.DataTypes where

data Person = Person String String Gender
              deriving Show

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data Manufacturer = Manufacturer String
                  deriving Show

data Model = Model Integer deriving Show

data Name = Name String deriving Show

data PastTravel = CanPastTravel | CannotPastTravel
                deriving Show

data FutureTravel = CanFutureTravel | CannotFutureTravel
                  deriving Show

data Price = Price Double deriving Show

data TimeMachine = TimeMachine {
                     manufacturer :: Manufacturer,
                     model :: Model,
                     name :: Name,
                     pastTravel :: PastTravel,
                     futureTravel ::FutureTravel,
                     price :: Price }
                 deriving Show

clientName :: Client -> String
clientName client = case client of
                      GovOrg n -> n
                      Company n _ _ _-> n
                      Individual (Person fN lN _) _ -> fN ++ " " ++ lN

companyName :: Client -> Maybe String
companyName client = case client of
                       Company n _ _ _ -> Just n
                       _ -> Nothing

countGenders :: [Person] -> (Integer, Integer)
countGenders [] = (0,0)
countGenders (p : ps) = let (mC, fC) = countGenders ps
                        in case p of
                             Person _ _ Male -> (mC + 1, fC)
                             Person _ _ Female -> (mC, fC + 1)
                             _ -> (mC, fC)

applyDiscount :: Double -> [TimeMachine] -> [TimeMachine]
applyDiscount _ [] = []
applyDiscount d (tm : tms) =
  let TimeMachine { price = (Price pr) } = tm
      disc = d * pr / 100
      newP = Price (pr - disc)
  in tm { price = newP } : (applyDiscount d tms)

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr Aljandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

data ClientR = GovOrgR { clientRName :: String}
             | CompanyR { clientRName :: String,
                          companyId :: Integer,
                          person :: PersonR,
                          duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String,
                         lastName :: String }
             deriving Show
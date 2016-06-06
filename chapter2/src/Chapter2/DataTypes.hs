module DataTypes where

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

data TimeMachine = TimeMachine
                     Manufacturer
                     Model
                     Name
                     PastTravel
                     FutureTravel
                     Price
                 deriving Show

clientName :: Client -> String
clientName client = case client of
                      GovOrg name -> name
                      Company name _ _ _-> name
                      Individual (Person fN lN _) _ -> fN ++ " " ++ lN

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
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
  let (TimeMachine man model name p f (Price pr)) = tm
      disc = d * pr / 100
      newP = Price (pr - disc)
  in (TimeMachine man model name p f newP) : (applyDiscount d tms)

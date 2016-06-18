module Client where

data Person = Person {firstName :: String, lastName :: String}
            deriving Show

data Client i = GovOrg {clientId :: i, clientName :: String}
              | Company {clientId :: i, clientName :: String,
                         person :: Person, duty :: String}
              | Individual {clientId :: i, person :: Person}
              deriving Show

class Nameable a where
  name :: a -> String

instance Nameable (Client i) where
  name Individual {person = Person {firstName = f, lastName = n}} = f ++ " " ++ n
  name c = clientName c

initial :: Nameable a => a -> Char
initial = head . name

instance Eq Person where
  (Person fName1 lName1) == (Person fName2 lName2) =
    fName1 == fName2 && lName1 == lName2

instance Eq i => Eq (Client i) where
  c1 == c2 = (clientId c1) == (clientId c2)
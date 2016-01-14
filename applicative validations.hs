-- inspired by function in PureScript
-- https://leanpub.com/purescript/read#leanpub-auto-applicative-validation

module Main where

import Control.Applicative

-- class Applicative
--pure :: a -> f a
--Lift a value.

--(<*>) :: f (a -> b) -> f a -> f b
--Sequential application.

--(*>) :: f a -> f b -> f b
--Sequence actions, discarding the value of the first argument.

--(<*) :: f a -> f b -> f a
--Sequence actions, discarding the value of the second argument.

data Address = Address String String String deriving Show

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone deriving Show

newtype PhoneNumber = PhoneNumber String deriving Show

data Person = Person {
  firstName :: String,
  lastName :: String,
  address :: Address,
  phones :: [PhoneNumber]
} deriving Show


a1 = Address "Broadway" "NYC" "USA"
-- Address "Broadway" "NYC" "USA"

a2 = liftA3 Address (Just "Broadway") (Just "NYC") (Just "USA")
-- Just (Address "Broadway" "NYC" "USA")

a3 = liftA3 Address (Just "Broadway") Nothing (Just "USA")
-- Nothing


(<?>) :: Maybe a -> b -> Either b a
(<?>) Nothing err = Left err
(<?>) (Just a) _ = Right a

fullAddress street city country = 
  Address <$> (street <?> "Wat? No street!")
          <*> (city <?> "No city")
          <*> (country <?> "No country!!")

a4 = fullAddress (Just "Broadway") (Just "NYC") (Just "USA")
-- Just (Address "Broadway" "NYC" "USA")

a5 = fullAddress (Just "Broadway") Nothing (Just "USA")
-- Left "No city"

a6 = fullAddress Nothing Nothing (Just "USA")
-- Left "Wat? No street!"


fullAddressAllErros street city country = 
  sequenceA [(street <?> "Wat? No street!")
          , (city <?> "No city")
          , (country <?> "No country!!")]


a7 = fullAddressAllErros Nothing Nothing (Just "USA")

nonEmpty :: String -> Either String ()
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _  = Right ()

-- x :: Person -> Either String ()
x p = (nonEmpty . firstName) p
w p = nonEmpty $ firstName p

validatePerson :: Person -> Either String Person
validatePerson p =
  Person <$> (nonEmpty (firstName p) *> pure (firstName p))
         <*> (nonEmpty (lastName p) *> pure (lastName p))
         <*> pure (address p)
         <*> pure (phones p)

p1 = validatePerson (Person "Fero" "Kocun" a1 [])
-- Right (Person {firstName = "Fero", lastName = "Kocun", address = Address "Broadway" "NYC" "USA", phones = []})

p2 = validatePerson (Person "Fero" "" a1 [])
-- Left "Field cannot be empty"

p3 = validatePerson (Person "" "" a1 [])
-- Left "Field cannot be empty"

data AccValidation e a = Failure {error :: [e]} | Success {value :: a} deriving Show

instance Functor (AccValidation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative (AccValidation e) where
  pure a = Success a
  (Failure e) <*> (Failure x) = Failure (e ++ x)
  (Failure e) <*> _           = Failure e
  (Success f) <*> (Failure e) = Failure e
  (Success f) <*> (Success a) = Success (f a)

accNonEmpty :: String -> String -> AccValidation String ()
accNonEmpty f "" = Failure ["Field " ++ f ++ " cannot be empty"]
accNonEmpty f _  = Success ()

accValidatePerson :: Person -> AccValidation String Person
accValidatePerson p =
  Person <$> (accNonEmpty "firstName" (firstName p) *> pure (firstName p))
         <*> (accNonEmpty "lastName" (lastName p) *> pure (lastName p))
         <*> pure (address p)
         <*> pure (phones p)

ap1 = accValidatePerson (Person "Fero" "Kocun" a1 [])
-- Success (Person {firstName = "Fero", lastName = "Kocun", address = Address "Broadway" "NYC" "USA", phones = []})

ap2 = accValidatePerson (Person "Fero" "" a1 [])
-- Failure {error = ["Field lastName cannot be empty"]}

ap3 = accValidatePerson (Person "" "" a1 [])
-- Failure {error = ["Field firstName cannot be empty","Field lastName cannot be empty"]}

(<??>) :: Maybe a -> b -> AccValidation b a
(<??>) Nothing err = Failure [err]
(<??>) (Just a) _ = Success a

lengthIs :: String -> Int -> String -> AccValidation String String
lengthIs field len value | length value /= len = 
  Failure ["Field '" ++ field ++ "' must have length " ++ show len]
lengthIs _ _ a = pure a

fromJust (Just a) = a
fromJust Nothing = ""

(<||>) :: AccValidation b a -> AccValidation b a -> AccValidation b a
f@(Failure _) <||> _ = f
_             <||> a = a

accFullAddress street city country = 
  Address <$> (street <??> "Wat? No street!")
          <*> (city <??> "No city")
          <*> (country <??> "No country!!" <||> (lengthIs "country" 3 (fromJust country)))

acca4 = accFullAddress (Just "Broadway") (Just "NYC") (Just "USA")
-- Success {value = Address "Broadway" "NYC" "USA"}

acca5 = accFullAddress (Just "Broadway") Nothing (Just "USA")
-- Failure {error = ["No city"]}

acca6 = accFullAddress Nothing Nothing (Just "USA")
-- Failure {error = ["Wat? No street!","No city"]}

acca7 = accFullAddress (Just "Broadway") Nothing Nothing
-- Failure {error = ["No city","No country!!"]}

acca8 = accFullAddress (Just "Broadway") Nothing (Just "United States of America")
-- Failure {error = ["No city","Field 'country' must have length 3"]}



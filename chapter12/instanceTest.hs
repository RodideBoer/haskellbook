module InstanceTest where

data PersonInvalid = NameEmpty
                   | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
    show = toString

instance Eq PersonInvalid where
    (==) NameEmpty NameEmpty = True
    (==) AgeTooLow AgeTooLow = True
    (==) _         _         = False

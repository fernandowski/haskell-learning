data CannotShow = CannotShow
                deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow
                      deriving (Show)


data OK = OK
instance Show OK where
    show _ = "Ok"

data ThisWorks = ThisWorks OK
               deriving (Show)
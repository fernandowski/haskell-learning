class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _  = False


data Color = Red | Green | Blue
     deriving (Read, Show, Eq, Ord)


-- instance Show Color where
--  show Red = "Red"
--  show Green = "Gree"
-- show Blue = "Blue"


-- instance Read Color where
--    readsPrec _ value =
--        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
--        where tryParse [] = []
--              tryParse ((attempt, result):xs) =
--                      if (take (length attempt) value) == attempt
--                         then [(result, drop (length attempt) value)]
--                         else tryParse xs


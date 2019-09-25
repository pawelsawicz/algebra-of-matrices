addRow : List Integer -> List Integer -> List Integer
addRow (a :: []) (b :: []) = [a + b]
addRow (a :: as) (b :: bs) = (a + b) :: (addRow as bs)
addRow _ _ = []

addMatrices : List (List Integer) -> List (List Integer) -> List (List Integer)
addMatrices (a :: [[]]) (b :: [[]]) = [addRow a b]
addMatrices (a :: as) (b :: bs) = (addRow a b) :: (addMatrices as bs)
addMatrices _ _ = [[]]

subtractRow : List Integer -> List Integer -> List Integer
subtractRow (a :: []) (b :: []) = [a - b]
subtractRow (a :: as) (b :: bs) = (a - b) :: (subtractRow as bs)
subtractRow _ _ = []

subtractMatrices : List (List Integer) -> List (List Integer) -> List (List Integer)
subtractMatrices (a :: [[]]) (b :: [[]]) = [subtractRow a b]
subtractMatrices (a :: as) (b :: bs) = (subtractRow a b) :: (subtractMatrices as bs)
subtractMatrices _ _ = [[]]

multRow : Integer -> List Integer -> List Integer
multRow k (a :: []) = [k * a]
multRow k (a :: as) = (k * a) :: multRow k as

multScalar : Integer -> List (List Integer) -> List (List Integer)
multScalar k (a :: as) = (multRow k a) :: (multScalar k as)
multScalar k _ = [[]]
rowOperation : (Integer -> Integer -> Integer) -> List Integer -> List Integer -> List Integer
rowOperation f (a :: []) (b :: []) = [f a b]
rowOperation f (a :: as) (b :: bs) = (f a b) :: (rowOperation f as bs)
rowOperation _ _ _ = []

matriceOperation : (Integer -> Integer -> Integer) -> List (List Integer) -> List (List Integer) -> List (List Integer)
matriceOperation f (a :: [[]]) (b :: [[]]) = [rowOperation f a b]
matriceOperation f (a :: as) (b :: bs) = (rowOperation f a b) :: (matriceOperation f as bs)
matriceOperation f _ _ = [[]]

multScalar : Integer -> List (List Integer) -> List (List Integer)
multScalar k (a :: as) = (map (*k) a) :: (multScalar k as)
multScalar k _ = [[]]
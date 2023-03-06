module Natural where

data Natural = Zero | Succ Natural
    deriving (Read, Eq, Ord)

instance Show Natural where
    -- kind of a cheat
    show :: Natural -> String
    show n = show $ fromEnum n

instance Num Natural where
    (+) :: Natural -> Natural -> Natural
    a + Zero = a
    a + (Succ b) = Succ a + b

    (*) :: Natural -> Natural -> Natural
    a * Zero = Zero
    a * (Succ b) = a + (a * b)

    abs :: Natural -> Natural
    abs n = n

    signum :: Natural -> Natural
    signum Zero = Zero
    signum _ = Succ Zero -- positive if gt zero

    negate :: Natural -> Natural
    negate = error "no additive inverse of a natural, silly :3"

    fromInteger :: Integral a => a -> Natural
    fromInteger z
        | z == 0    = Zero
        | z < 0     = fromIntegral $ abs z
        | z > 0     = Succ (fromIntegral z)

instance Enum Natural where
    enumFrom :: Natural -> [Natural]
    enumFrom n = n : enumFrom (Succ n)

    fromEnum :: Natural -> Int
    fromEnum Zero       = 0
    fromEnum (Succ n)   = 1 + fromEnum n

    toEnum :: Int -> Natural
    toEnum z
        | z == 0    = Zero
        | z < 0     = toEnum $ abs z
        | z > 0     = Succ $ toEnum (z - 1)

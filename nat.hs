import Prelude hiding ( (+), (-), (*), (/), (^) )
data Nat = Cero | Suc Nat
            deriving ( Eq, Ord, Show)

(+)           :: Nat -> Nat -> Nat
m + Cero   = m
m + Suc n = Suc (m + n) 

(-)           :: Nat -> Nat -> Nat
m - Cero   = m
Suc m - Suc n 
        | m < n  = Cero
        | m == n = Cero
        | m > n  = m - n

(*)           :: Nat -> Nat -> Nat
_ * Cero   = Cero
m * Suc n = (m * n) + m

{--
(/)           :: Nat -> Nat -> Nat
_ / Cero      = Cero
Cero / _      = Cero
Suc m / Suc n = (m / n)
--}

(^)           :: Nat -> Nat -> Nat
Suc _ ^ Cero  = Suc Cero
m ^ Suc n     = (m ^ n) * m
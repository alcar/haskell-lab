module Binary where

data Binary = Zero | One deriving Eq

instance Show Binary where
    show Zero = "0"
    show One  = "1"

type BinaryArray = [Binary] 

binaryAnd :: Binary -> Binary -> Binary
binaryAnd x y
  | x == One && y == One  = One
  | otherwise             = Zero

binaryOr :: Binary -> Binary -> Binary
binaryOr x y
  | x == Zero && y == Zero  = Zero
  | otherwise               = One

binaryXor :: Binary -> Binary -> Binary
binaryXor x y
  | x == y    = Zero
  | otherwise = One

binaryArrayCompliment :: [Binary] -> [Binary]
binaryArrayCompliment []     = []
binaryArrayCompliment (x:xs) = binaryBitCompliment x : binaryArrayCompliment xs where
    binaryBitCompliment Zero  = One
    binaryBitCompliment One   = Zero

binaryAdd :: [Binary] -> [Binary] -> Binary -> [Binary]
binaryAdd _         []      carry = []
binaryAdd (x:xs)    (y:ys)  carry = (bitSum x y carry) : binaryAdd xs ys (calculateCarry x y carry) where
    bitSum          x y carry = binaryXor carry $ binaryXor x y
    calculateCarry  x y carry = binaryOr (binaryAnd x y) $ binaryOr (binaryAnd y carry) (binaryAnd carry x)
binaryAdd x         y       carry = binaryAdd y x carry 

binaryAddArrays :: [Binary] -> [Binary] -> [Binary]
binaryAddArrays x y = reverse $ binaryAdd (reverse x) (reverse y) Zero

binarySubtract :: [Binary] -> [Binary] -> [Binary]
binarySubtract x y = binaryAdd x (binaryArrayCompliment y) One

binarySubtractArrays :: [Binary] -> [Binary] -> [Binary]
binarySubtractArrays x y = reverse $ binarySubtract (reverse x) (reverse y)

{-binaryMultiply :: [Binary] -> [Binary] -> Binary -> [Binary]-}

{-binaryMultiplyArrays :: [Binary] -> [Binary] -> Binary -> [Binary]-}
{-binaryMultiplyArrays x y carry = reverse $ binaryMultiply (reverse x) (reverse y) carry-}

{-binaryDivide :: [Binary] -> [Binary] -> Binary -> [Binary]-}

{-binaryDivideArrays :: [Binary] -> [Binary] -> Binary -> [Binary]-}
{-binaryDivideArrays x y carry = reverse $ binaryDivide (reverse x) (reverse y) carry-}

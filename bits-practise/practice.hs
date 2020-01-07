import Data.Bits

showBitVector :: Int -> Int -> String
showBitVector bv 0
  = ""
showBitVector bv n
  = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

setBits :: Int -> [Int] -> Int
setBits n xs
  = foldl setBit n xs

mask :: Int -> Int -> Int
mask
  = (.&.)

leastSigBits :: Int -> Int -> Int
leastSigBits n x
  = mask x (bit n - 1)

bitAt :: Int -> Int -> Int
bitAt n x
  = fromEnum $ testBit n x

zigZag :: Int -> Int -> Int
zigZag n m
  = (n .&. m .&. maskEven) .|. ((n .|. m) .&. maskOdd)
  where
    maxIndex = div (max n m) 2
    maskEven = foldl setBit zeroBits [0,2..maxIndex]
    maskOdd = foldl setBit zeroBits [1,3..maxIndex]

amalgam :: [Int] -> Int
amalgam xs
  = foldl (\x (y, n) -> x .|. (y .&. bit n)) zeroBits (zip xs [0..])

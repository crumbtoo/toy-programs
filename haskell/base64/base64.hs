import Data.Word
import Data.Bits
import Data.Char (ord)
import qualified Data.ByteString as BStr

sextetsOf :: [Int] -> [Int]
sextetsOf [] = []
sextetsOf (x:xs) = p : q : r : s : (sextetsOf xs)
    where p = (x `shiftR` 18) .&. sixLSB
          q = (x `shiftR` 12) .&. sixLSB
          r = (x `shiftR`  6) .&. sixLSB
          s = (x `shiftR`  0) .&. sixLSB
          sixLSB = 2^6-1

trioctetsOf :: String -> [Int]
trioctetsOf "" = []
trioctetsOf (a:b:c:st) = (j + k + l) : (trioctetsOf st)
    where j = (ord a) `shiftL` 16
          k = (ord b) `shiftL` 8
          l = (ord c) `shiftL` 0
-- only matches strings shorter than three characters
trioctetsOf s = trioctetsOf (s ++ "\00")

encodeSextets :: String -> String
encodeSextets s = map (\c -> alphabet !! c) ((sextetsOf.trioctetsOf) s)
    where alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

applyPadding :: String -> Int -> String
applyPadding s sourceLen = (take (length s - oppLenRem) s) ++ padding
    where padding = take (oppLenRem) (cycle "=")
          lenRem = sourceLen `mod` 3
          oppLenRem = 3 - lenRem


base64encode :: String -> String
base64encode s = applyPadding (encodeSextets s) (length s)

main = do
    let s = "ManMa"
    print (base64encode s)

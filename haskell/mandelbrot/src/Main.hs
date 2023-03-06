module Main where

import System.IO
import Control.Monad
import Text.Printf
import Data.Complex

type RGB = (Int, Int, Int)

width, height :: Num a => a
width = 16654
height = 8192

main :: IO ()
main = do
    let points = [ (x,y) | y <- [1..height], x <- [1..width] ]
    putStr $ mkHeader width height
    mapM_ (\ (x,y) ->
            let (r,g,b) = pointToRGB x y in
            putStrLn $ printf "%d %d %d # x: %d   y: %d" r g b x y
        ) points

pointToRGB :: Int -> Int -> RGB
pointToRGB x y =
    ( value
    , value
    , value
    )
    where
        c :: Complex Double
        c = rx :+ ry
        -- adjust to get the origin at the center of the image :3
        rx = (fromIntegral x / (height / 2)) - 2.5
        ry = (fromIntegral y / (height / 2)) - 1

        -- infinite list of mandelbrot iterations
        m = iterate (f c) 0

        isInfiniteC :: RealFloat a => Complex a -> Bool
        isInfiniteC n = ((isInfinite <$> n) == False :+ False)

        value =
            -- if it hasn't diverged after 50 iterations, colour it white
            if takeWhile isInfiniteC m `longerThan` 50
            then 255
            else 0

-- mandelbrot function
f :: (Num a, RealFloat a) => Complex a -> Complex a -> Complex a
f c z = z^2 + c

-- semantics: this will return true if length ls == n
longerThan :: [a] -> Int -> Bool
ls `longerThan` n = length (take n ls) == n

mkHeader :: Int -> Int -> String
mkHeader = printf "P3 %d %d 255\n"

-- showRGB :: RGB -> IO ()


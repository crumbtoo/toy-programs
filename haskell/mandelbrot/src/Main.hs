module Main where

import System.IO
import Control.Monad
import Text.Printf

type RGB = (Int, Int, Int)

width, height :: Num a => a
width = 128
height = 128

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
        cx, cy :: Float
        cx = width / 2.0
        cy = height / 2.0
        d = sqrt $ (cx - rx)^2 + (cy - ry)^2
        r = 20
        rx = fromIntegral x
        ry = fromIntegral y
        value = if d < r then 255 else 0

mkHeader :: Int -> Int -> String
mkHeader = printf "P3 %d %d 255\n"

-- showRGB :: RGB -> IO ()


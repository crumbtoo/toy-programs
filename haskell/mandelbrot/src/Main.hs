module Main where

import System.IO
import Control.Monad
import Text.Printf

type RGB = (Int, Int, Int)

width, height :: Num a => a
width = 1920
height = 1080

main :: IO ()
main = do
    let points = [ (x,y) | x <- [1..width], y <- [1..height] ]
    putStr $ mkHeader width height
    mapM_ (\ (x,y) ->
            let (r,g,b) = pointToRGB x y in
            putStrLn $ printf "%d %d %d" r g b
        ) points

pointToRGB :: Int -> Int -> RGB
pointToRGB x y =
    ( round $ 255 * (rx / width)
    , round $ 255 * (ry / height)
    , round $ 255 * (rx + ry / width + height)
    )
    where
        rx = fromIntegral x
        ry = fromIntegral y

mkHeader :: Int -> Int -> String
mkHeader = printf "P3 %d %d 255\n"

-- showRGB :: RGB -> IO ()


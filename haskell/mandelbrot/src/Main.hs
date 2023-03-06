module Main where

import System.IO
import Control.Monad
import Text.Printf
import Data.Complex
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

type RGB = (Int, Int, Int)

-- width, height :: Num a => a
-- width = 384
-- height = 256

data Flag 
    = Width Int
    | Height Int
    deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["height"] (ReqArg setHeight "INT") "image height"
    , Option ['w'] ["width"] (ReqArg setWidth "INT") "image width"
    ]

setWidth, setHeight :: String -> Flag
setWidth s = Width $ (read s :: Int)
setHeight s = Height $ (read s :: Int)

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
       (o,n,[]  ) -> return (o,n)
       (_,_,errs) -> ioError (userError $ concat errs)
 where header = ""

getwh :: [Flag] -> (Int, Int)
getwh [] = (384, 256)
getwh (x:xs) =
    case x of
        (Width w) -> (w, snd $ getwh xs)
        (Height h) -> (fst $ getwh xs, h)

main :: IO ()
main = do
    (flags, _) <- (getArgs >>= compilerOpts)

    -- print flags
    -- print $ getwh flags

    let (width,height) = getwh flags
    let points = [ (x,y) | y <- [1..height], x <- [1..width] ]

    putStr $ mkHeader width height
    mapM_ (\ (x,y) ->
            let (r,g,b) = pointToRGB width height x y in
            putStrLn $ printf "%d %d %d # x: %d   y: %d" r g b x y
        ) points

pointToRGB :: Int -> Int -> Int -> Int -> RGB
pointToRGB width height x y =
    ( value
    , value
    , value
    )
    where
        c :: Complex Double
        c = rx :+ ry
        -- adjust to get the origin at the center of the image :3
        rx = fromIntegral x / (fromIntegral width/3) - 2.5
        ry = fromIntegral y / (fromIntegral height/2) - 1

        -- infinite list of mandelbrot iterations
        m = iterate (f c) 0

        isInfiniteC :: RealFloat a => Complex a -> Bool
        isInfiniteC n = ((isInfinite <$> n) == False :+ False)

        noninf = takeWhile isInfiniteC m

        value
            -- appears non-divergent
            | noninf `longerThan` 50     = 0
            -- possibly divergent
            | otherwise                  = round $ 255 * (fromIntegral (length noninf) / 50)

-- mandelbrot function
f :: (Num a, RealFloat a) => Complex a -> Complex a -> Complex a
f c z = z^2 + c

-- semantics: this will return true if length ls == n
longerThan :: [a] -> Int -> Bool
ls `longerThan` n = length (take n ls) == n

mkHeader :: Int -> Int -> String
mkHeader = printf "P3 %d %d 255\n"

-- showRGB :: RGB -> IO ()


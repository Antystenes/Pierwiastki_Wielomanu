import Data.List (break, dropWhile, nub)
import Data.Char (isDigit)
import Data.Ratio
import Control.Applicative
import Control.Arrow

-- decrypt 1x^4 -1x^3 1x^2 -3x^1 -6x^0

--solve ::
solve s =
    let dcrp    = decrypt s
        targets = getPossibilities (fst . head $ dcrp) (fst . last $ dcrp)
--        foo a   = foldingFunction a dcrp
--        zippedV = (,) <$> targets <*> dcrp

--        hit     = foldl (\acc x -> (fst $ snd x) * (fst x) ^ (snd $ snd x) + acc) 0
--    in  hit <$> zippedV
--    in (\x y z -> (x,(y,z))) <$> fst <*> (fst . snd) <*> (snd . snd) <$> zippedV
--    in bar dcrp targets
    in bar dcrp targets

divisors :: Int -> [Int]
divisors x = [ a | a <- [1..abs(x)], (x `rem` a) == 0]

decrypt :: String -> [(Int,Int)]
decrypt s =
    let worded   = words s
        brokenUp = fmap (break (=='x')) worded
        powers   = fmap (dropWhile (not . isDigit)) $ fmap snd brokenUp
        handled1 = fmap (handleNeg . fst) brokenUp
        handled2 = fmap handleNeg powers
        paired   = zip handled1 handled2
    in  paired

handleNeg :: String -> Int
handleNeg a@(x:xs)
        | x == '-'  = (negate . read) xs
        | x == '+'  = read xs
        | otherwise = read a

getPossibilities :: Int -> Int -> [Ratio Int]
getPossibilities p q = nub $ (%) <$> (divisors q) <*> (divisors p)

--foldingFunction :: [(a, a)] -> Ratio Int -> Ratio Int
--foldingFunction dcrptd trgt = foldl (\acc x -> (fst x) * trgt ^ (snd x) + acc) 0 dcrptd

-- THIS SHIT IS WHY I WANT TO DIE

p n = foldr (\x a -> x * a) 1 . replicate n

foo ::[(Ratio Int,Int)] -> Ratio Int -> Ratio Int
foo l x = foldr (\(a,b) acc -> p b (a * x) + acc) 0 l
  --fst ab * x ^ snd ab + foo abes x

bar :: [(Int,Int)] -> [Ratio Int] -> [Ratio Int]
--bar abes (x:xs) = foo abes x : bar abes xs
--bar _ []        = []

bar = map . foo . map (first fromIntegral)

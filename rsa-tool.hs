{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System
import Data.List (elemIndex)
import Monad (join)
import Control.Monad (liftM)
import Data.Maybe (isJust, isNothing, fromJust)
import System.Random
import qualified Data.Map as Map

data Key = PublicKey Integer Integer | PrivateKey Integer Integer
	deriving Show

data Flag = BreakPrivateKey | Verbose | CheckConf
	deriving (Show, Eq)

data Configuration = Configuration {
		flags :: [Flag],
		publicKey :: Maybe Key,
		privateKey :: Maybe Key,
		phase :: Int,
		primeRange :: (Integer, Integer),
		input :: String
	}

instance Show Configuration where
	show (Configuration fs pub priv l r i) =
		"Flags given: "++(show fs)++"\n"++
		(show pub)++"\n"++(show priv)++"\n"++
		"Phase length: "++(show l)++"\n"++
		"Primes generated in range "++(show r)++"\n"++
		"Given message: "++i

type Parser a b = a -> b -> (a, b)

pPlus :: Parser a b -> Parser a b -> Parser a b
parser1 `pPlus` parser2 = \q w -> let (q', w') = parser1 q w; (q'', w'') = parser2 q' w'; in (q'',w'')

chars = ['A'..'Z']++['a'..'z']++['Ä','Ö','Å','ä','ö','å','!','?','.',',',' ','_']++['0'..'9']

intLength :: (Num a) => a -> Int
intLength = length . show

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod 0 _ _ = 0
powerMod _ _ 0 = 0
powerMod _ _ 1 = 0
powerMod 1 _ _ = 1
powerMod _ 0 _ = 1
powerMod b e m =
	case e `mod` 2 of
		0 -> powerMod (b*b `mod` m) (e `div` 2) m
		1 -> (b * powerMod (b*b `mod` m) (e `div` 2) m) `mod` m

euclid :: Integer -> Integer -> [(Integer, Integer)]
euclid a b = [(x0+n*b `quot` (gcd a b), y0-n*a `div` (gcd a b))|n<-[1..]]
	where (x0,y0) = euclidSingleAns a b

euclidSingleAns :: Integer -> Integer -> (Integer, Integer)
euclidSingleAns a b
	| a `mod` b == 0 = (0,1)
	| otherwise = (y, x-(y*(a `quot` b)))
	where (x, y) = euclidSingleAns b (a `mod` b)

toNums :: String -> [Integer]
toNums "" = []
toNums (m:ms)
	| isNothing i = toNums ms
	| otherwise = (fromJust i):(toNums ms)
	where i = liftM ((+2) . fromIntegral )$ elemIndex m chars

fromNums :: [Integer] -> String
fromNums [] = []
fromNums (n:ns) = (chars !! (fromIntegral (n-2))):(fromNums ns)

cut :: Int -> [a] -> [[a]]
cut _ [] = []
cut l ns = (take l ns):(cut l (drop l ns))

toGroups :: Int -> [Integer] -> [Integer]
toGroups _ [] = []
toGroups l ns = map (foldl (\a b->a*100+b) 0) (cut l ns)

fromGroups :: [Integer] -> [Integer]
fromGroups [] = []
fromGroups ns = map read (join (map (\n->cut 2 (show n)) ns)) :: [Integer]

coprimes :: Integer -> [Integer]
coprimes p = [x | x<-[2..], gcd p x == 1]

isPrime :: Integer -> Bool
isPrime n = n > 1 && n == head (primeFactors n)

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = go n primes
	where
		go n ps@(p:pt)
			| p*p > n = [n]
			| n `rem` p == 0 = p : go (n `quot` p) ps
			| otherwise = go n pt

primes :: [Integer]
primes = 2:3:primes'
	where
		1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
		primes' = p : filter isPrime candidates
		isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
		divides n p = n `mod` p == 0

primesBetween :: (Integer,Integer) -> [Integer]
primesBetween (a,b) = dropWhile (>a) $ takeWhile (<b) primes

breakPrivateKey :: Key -> Key
breakPrivateKey (PublicKey e n) = PrivateKey (fst $ euclid e ((p-1)*(q-1)) !! 0) n
	where p:q:_ = primeFactors n

generateKeys :: Integer -> Integer -> (Key, Key)
generateKeys p q = (PublicKey e n, PrivateKey d n)
	where
		n = p*q
		n' = (p-1)*(q-1)
		e = (coprimes n') !! 0
		d = fst $ euclid e n' !! 0

encrypt :: Key -> Int -> String -> String
encrypt _ _ [] = []
encrypt (PublicKey e n) l msg = toCryptotext crypted (intLength n)
	where
		crypted = map (\m->powerMod m e n) msg'
		msg' = ((toGroups l) . toNums) msg

decrypt :: Key -> String -> String
decrypt _ [] = ""
decrypt (PrivateKey d n) msg = (fromNums . fromGroups) $ map (\m->powerMod m d n) msg'
	where msg' = fromCryptotext msg (intLength n)

fromCryptotext :: String -> Int -> [Integer]
fromCryptotext msg l = map step (cut l msg)
	where
		step ('0':x) = step x
		step x = (read x)::Integer

toCryptotext :: [Integer] -> Int -> String
toCryptotext msg l = foldr (++) "" $ map step msg
	where step x | ml < l = (take (l-ml) ['0','0'..])++m | otherwise = m where m = show x; ml = length m

readInteger = \s->(read s)::Integer

readInt = \s->(read s)::Int

addFlag flag conf = conf { flags = flag:(flags conf) }

defaultConf = Configuration [] Nothing Nothing 3 (1000,500000) ""

parsePublicKey :: Parser [String] Configuration
parsePublicKey (e:n:xs) conf = (xs, conf { publicKey = Just $ PublicKey (readInteger e) (readInteger n) })

parsePrivateKey (d:n:xs) conf = (xs, conf { privateKey = Just $ PrivateKey (readInteger d) (readInteger n) })

parseMessage xs conf = ([], conf { input = join xs } )

parseVerbose xs conf = (xs, addFlag Verbose conf)

parseBreakPriv xs conf = (xs, addFlag BreakPrivateKey conf)

parsePhase (x:xs) conf = (xs, conf { phase = readInt x })

parseRange (a:b:xs) conf = (xs, conf { primeRange = (readInteger a, readInteger b) })

parsePrimes (p:_:q:xs) conf = (xs, conf { publicKey = Just pub, privateKey = Just priv })
	where (pub, priv) = generateKeys (readInteger p) (readInteger q)

argParsers :: Map.Map String (Parser [String] Configuration)
argParsers = Map.fromList [
			("--publicKey", parsePublicKey),
			("--privateKey", parsePrivateKey),
			("--message", parseMessage),
			("--verbose", parseVerbose),
			("--breakPrivateKey", parseBreakPriv),
			("--phase", parsePhase),
			("--range", parseRange),
			("-a", parsePublicKey),
			("-b", parsePrivateKey),
			("-m", parseMessage),
			("-v", parseVerbose),
			("-B", parseBreakPriv),
			("-l", parsePhase),
			("-p", parsePrimes),
			("-r", parseRange)
			]

parseConfiguration xs = snd $ parseConfiguration' xs defaultConf
	where
		parseConfiguration' :: Parser [String] Configuration
		parseConfiguration' [] conf = ([], conf)
		parseConfiguration' (x:xs) conf = ((fromJust $ Map.lookup x argParsers) `pPlus` parseConfiguration') xs conf

output :: Configuration -> String -> IO String
output (Configuration _ Nothing Nothing _ _ "") fallback = return fallback
output conf@(Configuration fs (Just pub) _ _ _ "") _
	| BreakPrivateKey `elem` fs = return $ show $ conf { privateKey = Just $ breakPrivateKey pub }
output conf@(Configuration fs Nothing Nothing l range msg) _ = do
	let primes' = primesBetween (primeRange conf)
	a <- randomRIO (0::Int, length primes')
	b <- randomRIO (0::Int, length primes')
	let	p = primes' !! a; q = primes' !! b
		(pub, priv) = generateKeys p q
		msg = encrypt pub (phase conf) (input conf)
	return $ (show conf { privateKey = Just priv, publicKey = Just pub })++"\n"++msg

output conf@(Configuration fs (Just pub) Nothing l _ msg) _ = do
	let encryptedText = encrypt pub l msg
	if (Verbose `elem` fs) then return $ (show conf)++"\n"++encryptedText else return encryptedText
output conf@(Configuration fs Nothing (Just priv) _ _ msg) _ = do
	let decryptedText = decrypt priv msg
	if (Verbose `elem` fs) then return $ (show conf)++"\n"++decryptedText else return decryptedText
output conf@(Configuration fs (Just pub) (Just priv) l _ msg) _ = do
	let encryptedText = encrypt pub l msg
	if (Verbose `elem` fs) then return $ (show conf)++"\n"++encryptedText else return encryptedText

main = do
	args <- getArgs
	let conf = parseConfiguration args
	fallback <- readFile "helptext.txt"
	(output conf fallback) >>= putStrLn

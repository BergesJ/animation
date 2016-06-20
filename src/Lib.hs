module Lib where

import Data.Maybe
import Data.List 
import Data.Char
import Data.Fixed
import Graphics.Gloss
import Graphics.Gloss.Data.Color 

zugig :: IO ()
zugig = do 
 putStrLn "Please type in filepath for log-data."
 file <- getLine 
 text <- readFile file
 ani $ parse (many1 zugParser) text


---bar chart---
barChart :: [([Zugriff],String)] -> [Picture]
barChart = balken.buckets24.water

water :: [([Zugriff],String)] -> [(Int,Int)]
water = h2o.fst.head

h2o :: [Zugriff] -> [(Int,Int)]
h2o l = zip (fmap (hours.time) l) (fmap (const 1) l)

buckets24 :: Num a => [(Int,Int)] -> [(Int,a)]
buckets24 l = [(h,fromIntegral $ length $ filter ((==h).fst) l)| h <- [0..23]]

balken :: [(Int,Float)] -> [Picture]
balken l = [translate ((fromIntegral x)*30-345) (y/2 - 200) $ rectangleSolid 10 y | (x,y) <- l] 
           ++ [translate ((fromIntegral x)*30-350) (-215) $ scale (0.08) (0.08) $ text $ show x | (x,y) <- l] 
           ++ [translate ((fromIntegral x)*30-350) (y-185) $ scale (0.08) (0.08) $ text $ take 3 $ show y | (x,y) <- l] 


---animation---
ani :: [([Zugriff],String)] -> IO ()
ani = \l -> (animate (InWindow "Zugriffe" (1800,1000) (50,0)) 
              (white)
              (\time -> pictures $ (fmap (translate (500) 0) $ barChart l) ++ (fmap ((translate (-500) 0).snd) $ filter (\a -> (fst a)<=(time*360) `div'` 300) $ fmap kachel $ buckets l)))
              
buckets :: [([Zugriff],String)] -> [(Int,Int)]
buckets l = zip (fmap (bucketTime.time) $ fst $ head l) (fmap (oct1.ip) $ fst $ head l) 
 where bucketTime t = (hours t*3600+minutes t*60+seconds t) `div` 300

kachel :: (Int,Int) -> (Int,Picture)
kachel = \(t,ip) -> (t,translate (fromIntegral $ ip `mod` 16 * 50 - 325) (fromIntegral $ ip `div` 16 * 50 - 325) $ color (makeColor (1) (0) (0) (0.04)) (rectangleSolid 49.0 49.0))


---parser---
newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

class Monad m => MonadZero m where 
 mzero :: m a 

class MonadZero m => MonadPlus m where 
 mplus :: m a -> m a -> m a 

class Applicative f => Alternative f where
 empty :: f a
 (<|>) :: f a -> f a -> f a

instance Functor Parser where 
 fmap f p  = Parser (\cs -> [(f a,cs') | (a,cs') <- parse p cs])

instance Applicative Parser where 
 pure a =  Parser (\cs -> [(a,cs)])
 p <*> q = Parser (\cs -> [(f a,cs') | (f,cs') <- parse p cs, (a,cs') <- parse q cs ])

instance Monad Parser where 
 return a = Parser (\cs -> [(a,cs)])
 p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadZero Parser where 
 mzero = Parser (\cs -> [])

instance MonadPlus Parser where 
 mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = Parser (\cs -> case parse (mplus p q) cs of 
                                []     -> []
                                (x:xs) -> [x])

instance Alternative Parser where 
 empty = mzero
 (<|>) p1 p2 = Parser $ \xs -> case parse p1 xs of 
                                []  -> parse p2 xs 
                                _   -> parse p1 xs

item :: Parser Char 
item = Parser (\xs -> case xs of 
                         ""      -> []
                         (c:cs) -> [(c,cs)])

many :: Parser a -> Parser [a]
many p = many1 p +++ return [] 

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)} 

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = (end >> return []) <|> do {x <- p; xs <- (manyTill p end); return (x:xs)}

sat :: (Char -> Bool) -> Parser Char 
sat pred = do {c <- item; if pred c then return c else mzero} 

char :: Char -> Parser Char 
char c = sat (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

whitespace :: Parser Char
whitespace = (char ' ') <|> (char '\t')

---logParser--- 
data Date = Date { year  :: Int
                 , month :: Int
                 , day   :: Int }
 deriving Show

data Time = Time { hours   :: Int
                 , minutes :: Int
                 , seconds :: Int } 
 deriving Show

data IP = IP Int Int Int Int
 deriving Show

data Zugriff = Zugriff { date :: Date
                       , time :: Time
                       , ip   :: IP   } 
 deriving Show

oct1 :: IP -> Int 
oct1 (IP oct _ _ _) = oct

dateParser :: Parser Date 
dateParser = do 
 y1000 <- sat isDigit
 y0100 <- sat isDigit
 y0010 <- sat isDigit
 y0001 <- sat isDigit
 char '-'
 m10 <- sat isDigit
 m01 <- sat isDigit
 char '-'
 d10 <- sat isDigit
 d01 <- sat isDigit
 return $ Date ((digitToInt y1000) * 1000 + (digitToInt y0100) * 100 + (digitToInt y0010) * 10 + (digitToInt y0001)) ((digitToInt m10) * 10 + digitToInt m01) ((digitToInt d10) * 10 + digitToInt d01)

timeParser :: Parser Time 
timeParser = do 
 char 'T'
 h10 <- sat isDigit
 h01 <- sat isDigit
 char ':'
 m10 <- sat isDigit
 m01 <- sat isDigit
 char ':'
 s10 <- sat isDigit
 s01 <- sat isDigit
 char 'Z'
 return $ Time ((digitToInt h10) * 10 + digitToInt h01) ((digitToInt m10) * 10 + digitToInt m01) ((digitToInt s10) * 10 + digitToInt s01)

octParser :: Parser [Int]
octParser = Parser f 
 where f "" = []
       f s  = [(fmap digitToInt $ takeWhile isDigit s , dropWhile isDigit s)]

ipParser :: Parser IP 
ipParser = do 
 oct1 <- octParser
 char '.'
 oct2 <- octParser
 char '.'
 oct3 <- octParser
 char '.'
 oct4 <- octParser
 return $ IP (toInt oct1) (toInt oct2) (toInt oct3) (toInt oct4)

toInt :: [Int] -> Int 
toInt [] = 0
toInt l  = 10^(length l - 1) * (head l) + toInt (tail l) 

zugParser :: Parser Zugriff 
zugParser = do 
 d <- dateParser 
 t <- timeParser
 many whitespace
 char ','
 many whitespace
 ip <- ipParser 
 char '\n'
 return $ Zugriff d t ip











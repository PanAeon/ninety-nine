module Main where

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import Text.Parsec(Parsec)
import qualified Text.Parsec as Parsec
import Text.Parsec.Combinator

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)
import qualified Data.Bits as Bits
import Control.Monad(mfilter)
import Data.List

--parse rule text = Parsec.parse rule "(source)" text

numberP :: Parsec String () Int
numberP = read <$> Parsec.many1 (Parsec.oneOf ['0'..'9'])

-- ipRange :: Parsec String () Int
-- ipRange = (mfilter (\x -> (x > 0) && (x<256)) numberP) <?> "IP range invalid"

ipFragmentP = do
  n <- numberP
  if n >= 0 && n < 256
  then (pure n)
  else fail $ "invalid ip fragment: " ++ show n

-- FIXME: find bloody combinator filterM or whatever!
cidrBitsP = do
  n <- numberP
  if n > 0 && n < 33
  then (pure n)
  else fail $ "invalid number of cidr bits: " ++ show n

data Ip = Ip  Int Int Int Int  deriving (Eq,Show)
data CidrBlock = CidrBlock Ip Int deriving (Eq,Show)
data IpRange = IpRange Ip Ip deriving (Eq,Show)
data IpRangeByNum = IpRangeByNum Ip Int deriving (Eq, Show)

ipP = Ip <$> ipFragmentP <* Parsec.char '.' <*>
                           ipFragmentP <* Parsec.char '.' <*>
                           ipFragmentP <* Parsec.char '.' <*>
                           ipFragmentP

cidrBlockP = CidrBlock <$> ipP <* Parsec.char '/' <*> cidrBitsP
ipRangeP = IpRange <$> ipP <* Parsec.char '-' <* ipP
ipRangeByNumP = IpRangeByNum <$> ipP <* Parsec.spaces <* numberP

-- aha, yes, bits not in mask should be 1
-- i.e. 255, for all masked.. count only unmasked...
-- here's good calculator http://subnet-calculator.org/cidr.php
-- FIXME: cidrToIpRange (CidrBlock (Ip 1 2 3 4 ) 23)
-- fuck: fix start adddr
incrIps :: Ip -> Int -> (Ip,Ip)
incrIps (Ip a b c d) nBits = (Ip a'' b'' c'' d'', Ip a' b' c' d')
  where
    n1 = nBits `div` 8
    n2 = nBits `mod` 8
    xs = [d, c, b, a]
    (as, (z:zs)) = splitAt n1 xs
    z'' = max 0 (z - 2^n2 + 1)
    z' = min 255 (z'' + 2^n2 - 1)
    [d', c', b', a'] = ((const 255) <$> as) ++ (z':zs)
    [d'', c'', b'', a''] = ((const 0) <$> as) ++ (z'':zs)



cidrToIpRange :: CidrBlock -> IpRange
cidrToIpRange (CidrBlock (Ip a b c d) n) = IpRange s' e'
  where
    startAddr = Ip a b c d
    bitsLeft  = 32 - n
    (s', e') = incrIps startAddr bitsLeft




main :: IO ()
main = do
         putStrLn "hi"

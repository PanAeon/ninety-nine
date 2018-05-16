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
  if n >= 0 && n < 33
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

-- FIXME: check for overflow
-- ha, you're wrong))
incrIps :: Ip -> Int -> Ip
incrIps (Ip a b c d) n = Ip a' b' c' d'
  where
    d' = min 255 (n + d)
    rd = max 0 (n + d - 255)
    c' = min 255 (rd + c)
    rc = max 0 (rd + c - 255)
    b' = min 255 (rc + b)
    rb = max 0 (rc + b - 255)
    a' = min 255 (rb + a)
    ra = max 0 (rb + a - 255) -- check for oferflow here..

cidrToIpRange :: CidrBlock -> IpRange
cidrToIpRange (CidrBlock (Ip a b c d) n) = undefined
  where
    startAddr = Ip a b c d
    bitsLeft  = 32 - n
    numIps = 2^bitsLeft



main :: IO ()
main = do
         putStrLn "hi"

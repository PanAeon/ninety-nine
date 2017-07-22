
module Ex70to73 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse)
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App



import Data.Tree


countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node a (x:xs)) = countNodes x + countNodes (Node a xs)


{- Problem 70

(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters.
In the depth-first order sequence of its nodes, a special character ^ has been
inserted whenever, during the tree traversal, the move is a backtrack
to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^


Define the syntax of the string and write a predicate tree(String,Tree) to construct
the Tree when the String is given. Make your predicate work in both directions.

-}
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
-- FIXME: use difference list
treeToString :: Tree Char -> String
treeToString (Node a xs) = a : (concatMap treeToString xs) ++ "^"

type Prsr a = String ->  (Maybe a,String)

-- FIXME: make your very own monadic parser, use MonadPlus
stringToTree :: String -> Tree Char
stringToTree = fromJust . fst . parseNode
        where
          parseNode:: Prsr (Tree Char)
          parseNode ('^':xs) = (Nothing,'^':xs)
          parseNode (x:xs)   = (Just (Node x chldrn), xs')
                      where
                        chldrn' = unfoldr f xs-- almost, need unfoldrM?
                        chldrn = map fst chldrn'

                        f s = fmap (\a -> ((a,ss), ss)) maybeA
                          where
                            (maybeA,ss) = parseNode s
                        ('^':xs') = if (null chldrn) then xs else (snd $ last chldrn')


-- Problem 71
-- Determine the internal path length of the tree
-- sum of all paths from root to a node

ipl :: Tree a -> Int
ipl t = ipl' t 0
      where
        ipl' (Node _ []) n = n
        ipl' (Node _ xs) n = n + (sum $ fmap (\x -> ipl' x (n+1)) xs)


{-  Problem 72

(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence
of the nodes of the multiway tree Tree.

 bottom_up tree5  => "gfcdeba"
-}

-- children --> parent
bottomUp :: Tree Char -> String
bottomUp (Node a xs) = concatMap bottomUp xs ++ [a]


-- Problem 73

-- (**) Lisp-like tree representation.

data Parser a = Parser {  runParser :: String ->  (Maybe a,String) }

instance  Functor Parser  where
    fmap f pa = Parser $ \s -> let
                                 (mb,s') = runParser pa s
                               in (fmap f mb, s')


instance Applicative Parser where
  pure = return
  (<*>) = ap
    -- Parser $ \s -> let
                            -- (ma, s') = runParser a s
                            -- (mb, s'') = Fldbl.foldl (\_ -> \a -> a)  (Nothing, s1) ma

instance Monad Parser where
   p0 >>= k = Parser (\s -> let
                               (maybeA, s1) = runParser p0 s
                               (maybeB, s2) = if (isJust maybeA) then
                                          runParser (k (fromJust maybeA)) s1
                                        else
                                          (Nothing, s1)
                            in (maybeB, s2)


                     )


   p0 >> k = Parser (\s -> let (_, s1) = runParser p0 s
                           in   runParser k s1

                    )
   return a = Parser (\s -> (Just a, s))
   fail   _ = Parser (\s -> (Nothing, s))

instance Alternative Parser where
  empty = Parser (\s -> (Nothing, s))

  ma <|> mb =  Parser $ \s -> let -- FIXME: this is not applicative...
                                (a, s1) = runParser ma s
                             in if (isJust a) then
                                          (a, s1)
                                        else
                                          runParser mb s
-- TODO: TEST ALL THE LAWS!
instance MonadPlus Parser where
  mzero = Parser (\s -> (Nothing, s))



  mplus ma mb = Parser $ \s -> let
                                (a, s1) = runParser ma s
                             in if (isJust a) then
                                          (a, s1)
                                        else
                                          runParser mb s




toLispNotation :: Tree Char -> String
toLispNotation (Node a []) = [a]
toLispNotation (Node a xs) = "(" ++ a:' ':(concat $ intersperse " " (fmap toLispNotation xs)) ++ ")"


parseChar :: Char -> Parser Char
parseChar c = Parser f
      where
        f (x:xs) | x == c    =  (Just x, xs)
                 | otherwise =  (Nothing, xs)
        f []                 =  (Nothing, [])


parseAlpha ::  Parser Char
parseAlpha = Parser f
      where
        f (x:xs) | elem x ['a'..'z']    =  (Just x, xs)
                 | otherwise =  (Nothing, xs)
        f []                 =  (Nothing, [])

skipSpace :: Parser ()
skipSpace = Parser f
      where
        f (x:xs) | x == ' '    =  (Just (), xs)
                 | otherwise =  (Just (), x:xs)
        f []                 =  (Just (), [])

skipChar :: Char ->  Parser ()
skipChar ch = Parser f
      where
        f (x:xs) | x == ch    =  (Just (), xs)
                 | otherwise =  (Just (), x:xs)
        f []                 =  (Just (), [])


treeParser :: Parser (Tree Char)
treeParser = nakedNode `mplus` fullNode

nakedNode :: Parser (Tree Char)
nakedNode = Parser f
      where
        f (x:xs) | elem x ['a'..'z'] = (Just $ Node x [], xs)
                 | otherwise         =  (Nothing, xs)
        f []                         = (Nothing, [])

separatedBy :: Char -> Parser (Tree Char) -> Parser [Tree Char]
separatedBy sep p = many ((skipChar sep) >> p)

fullNode :: Parser (Tree Char)
fullNode = do
                   parseChar '('
                   n <- parseAlpha
                   skipSpace
                   xs <- separatedBy ' ' treeParser
                   parseChar ')'
                   return $ Node n xs

fromLispNotation :: String -> Tree Char
fromLispNotation =  undefined

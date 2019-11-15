module Lib
  ( Cond(..)
  , pp
  ) where

import qualified Data.List       as L
import qualified Data.List.Split as LS
import qualified Data.Set        as S
import qualified Text.Printf     as P

type Tag = String

data Cond a
  = Cond Tag
         (S.Set a)
  | F [Cond a]
  | L [Cond a]
  | G Tag
      [a]
  | V Tag
      a
  | Or (Cond a)
       (Cond a)
  | And (Cond a)
        (Cond a)
  | Not (Cond a)
  deriving (Eq, Show)

pp :: (P.PrintfArg a, Show a) => Cond a -> String
pp (V t x) = eqOp t . quote . show $ x
pp (G t xs) =
  binOp "or" (inOp t . L.intercalate "\n," . map (quote . show)) .
  LS.chunksOf 1000 $
  xs
pp (L xs) = binOp "or" pp xs
pp (F xs) = binOp "and" pp xs
pp (Or cx cy) = op "or" pp cx cy
pp (And cx cy) = op "and" pp cx cy
pp (Cond t s) = pp . G t . S.toList $ s

op s p x y = P.printf "(%s %s %s)" (p x) s (p y)

binOp s p x = P.printf "(%s)" $ L.intercalate (" " ++ s ++ " ") . map p $ x

inOp t x = P.printf "(%s in (\n %s\n))" t x

eqOp t x = P.printf "(%s = %s)" t x

quote = P.printf "'%s'" . trim . concatMap repl

trim :: String -> String
trim = trimR . trimL

trimR :: String -> String
trimR = reverse . trimL . reverse

trimL :: String -> String
trimL []       = []
trimL ('"':xs) = xs
trimL xs       = xs

repl :: Char -> String
repl '\'' = "\\'"
repl c    = [c]

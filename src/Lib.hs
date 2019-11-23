module Lib
  ( Cond(..)
  , run
  , runE
  , pp
  , ppE
  , parseV
  , parseG
  , parseTag
  , parseOr
  , parseExpr
  , parseVal
  , parse
  ) where

import qualified Data.List                     as L
import qualified Data.List.Split               as LS
import qualified Data.Set                      as S
import qualified Text.Printf                   as P

import           Text.ParserCombinators.Parsec

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

run :: (P.PrintfArg a, Show a) => Parser (Cond a) -> String -> IO ()
run p input =
  case parse p "" input of
    Left err -> putStr "parse error at" >> print err
    Right x  -> putStrLn $ pp x

runE ::
     (P.PrintfArg a, Show a)
  => Parser (Cond a)
  -> String
  -> Either String String
runE p input =
  case parse p "" input of
    Left err -> Left $ "parse error at" ++ show err
    Right x  -> Right $ pp x

skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 space

skipSpaces :: Parser ()
skipSpaces = skipMany space

symbol :: Parser Char
symbol = oneOf "#!@$%^&{}|;:\"',.<>/?~`-_+="

underscore :: Parser Char
underscore = char '_'

escSymbol :: Parser Char
escSymbol = char '\\' >> oneOf "[]()"

parseTag :: Parser String
parseTag = char '\'' >> many1 (alphaNum <|> underscore)

parseVal :: Parser String
parseVal = many1 (alphaNum <|> symbol <|> escSymbol)

parseV :: Parser (Cond String)
parseV = do
  tag <- parseTag
  skipSpaces1
  v <- parseVal
  return . V tag $ v

parseG :: Parser (Cond String)
parseG = do
  tag <- parseTag
  skipSpaces >> char '[' >> skipSpaces
  vs <- sepEndBy1 parseVal spaces
  char ']'
  return . G tag $ vs

parseWord :: String -> Parser String
parseWord x = char '#' >> string x

parseTwo ::
     String
  -> (Cond String -> Cond String -> Cond String)
  -> Parser (Cond String)
parseTwo key op = do
  parseWord key
  fst <- parseExpr
  snd <- parseExpr
  return . op fst $ snd

parseOr :: Parser (Cond String)
parseOr = parseTwo "or" Or

parseAnd :: Parser (Cond String)
parseAnd = parseTwo "and" And

parseNot :: Parser (Cond String)
parseNot = Not <$> (parseWord "not" >> parseExpr)

parseSep :: Parser (Cond String)
parseSep = do
  char '('
  x <- parseExpr
  skipSpaces >> char ')'
  return x

parseExpr :: Parser (Cond String)
parseExpr =
  skipSpaces >>
  (try parseSep <|> try parseNot <|> try parseOr <|> try parseAnd <|> try parseG <|>
   try parseV)

ppE :: Either a (Cond String) -> String
ppE (Right c) = pp c

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
pp (Not x) = singleOp "not" $ pp x
pp (Cond t s) = pp . G t . S.toList $ s

opId :: String -> String
opId = P.printf "(%s)"

op s p x y = opId $ P.printf "%s %s %s" (p x) s (p y)

binOp s p = opId . L.intercalate (" " ++ s ++ " ") . map p

singleOp :: String -> String -> String
singleOp p = opId . P.printf (p ++ "\n%s")

inOp :: String -> String -> String
inOp t = opId . P.printf "%s in (\n %s\n)" t

eqOp :: String -> String -> String
eqOp t = opId . P.printf "%s = %s" t

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

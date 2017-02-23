module CS4430_HW1 where

import Parsing
import Data.String
import Data.Char

--
-- define what a token is
--
data Token = ASSIGN | WRITEI  | READI | ADDI | SUBI | JUMP | JNZ | LABEL
           | LITERAL String | LPAREN | RPAREN | LBRACKET | RBRACKET
           | COMMA | PLUSOP | ID String | R String | SP | BP | FP | M
           | SCANEOF
           deriving (Show, Read)

recognize :: String -> Token
recognize str = case str of
  "ASSIGN"  -> ASSIGN
  "WRITEI"  -> WRITEI
  "READI" -> READI
  "ADDI"  -> ADDI
  "SUBI"  -> SUBI
  "JUMP"  -> JUMP
  "JNZ" -> JNZ
  "LABEL" -> LABEL
  "SP"  -> SP
  "FP"  -> FP
  "M" -> M
  "BP"  -> BP
  _ -> recognizeRegister str

recognizeRegister :: String -> Token
recognizeRegister ('R':all@(x:xs)) = if isDigit x then R all else ID ('R':all)
recognizeRegister str = ID str

--
-- a test case
--

test = "(ASSIGN,BP,R99)\n (ASSIGN,SP,0)\n (JUMP,M[FP])\n (LABEL,3)"

run (P x) inp = x inp

opcode :: Parser Token
opcode = do
  i <- many alphanum
  return (recognize i)

assign :: Parser Token
assign = do
  i <- symbol "ASSIGN"
  return ASSIGN

writei :: Parser Token
writei = do
  i <- symbol "WRITEI"
  return WRITEI

readi :: Parser Token
readi = do
  i <- symbol "READI"
  return READI

addi :: Parser Token
addi = do
  i <- symbol "ADDI"
  return ADDI

subi :: Parser Token
subi = do
  i <- symbol "SUBI"
  return SUBI

jump :: Parser Token
jump = do
  i <- symbol "JUMP"
  return JUMP

jnz :: Parser Token
jnz = do
  i <- symbol "JNZ"
  return JNZ

label :: Parser Token
label = do
  i <- symbol "LABEL"
  return LABEL

sp :: Parser Token
sp = do
  i <- symbol "SP"
  return SP

fp :: Parser Token
fp = do
  i <- symbol "FP"
  return FP

bp :: Parser Token
bp = do
  i <- symbol "BP"
  return BP

m :: Parser Token
m = do
  i <- symbol "M"
  return M

register :: Parser Token
register = do
  char 'R'
  n <- many1 digit
  return (R (show (read n :: Integer)))

number :: Parser Token
number = do
  space
  n <- many1 digit
  space
  return (LITERAL (show (read n :: Integer)))

lparen :: Parser Token
lparen = do
  symbol "("
  return LPAREN

rparen :: Parser Token
rparen = do
  symbol ")"
  return RPAREN

lbracket :: Parser Token
lbracket = do
  symbol "["
  return LBRACKET

rbracket :: Parser Token
rbracket = do
  symbol "]"
  return RBRACKET

comma :: Parser Token
comma = do
  symbol ","
  return COMMA

plusop :: Parser Token
plusop = do
  symbol "+"
  return PLUSOP

-- Remove white spaces from the string/ etc.,
removeWhiteSpace [] = []
removeWhiteSpace ('\n':a) = removeWhiteSpace a
removeWhiteSpace (' ':a) = removeWhiteSpace a
removeWhiteSpace (a:b) = a : removeWhiteSpace b

lexer :: Parser Token
lexer = assign
          +++ writei
          +++ readi
          +++ addi
          +++ subi
          +++ jump
          +++ jnz
          +++ label
          +++ sp
          +++ fp
          +++ bp
          +++ m
          +++ register
          +++ number
          +++ lparen
          +++ rparen
          +++ lbracket
          +++ rbracket
          +++ comma
          +++ plusop

--   data Maybe a = Just a | Nothing

tuplelex :: String -> Maybe [Token]
tuplelex inp = case run (many lexer) (removeWhiteSpace inp) of
                    [(toks,"")] -> Just (toks ++ [SCANEOF])
                    _           -> Nothing

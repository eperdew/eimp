{-# LANGUAGE OverloadedStrings #-}

module Parse
( pAexp
, pBexp
, pStmt
, printStore
, parseFile
, parseAndShowFile
, evalFile
) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.Except
import Control.Monad
import Data.Text (Text)
import Data.Text.IO as TextIO hiding (putStrLn)
import qualified Data.Map as Map
import Data.Void
import Data.Either

type Parser = Parsec Void Text
type StmtParseError = ParseErrorBundle Text Void

{- Main interesting functions to call from the top level -}

parseFile :: FilePath -> ExceptT StmtParseError IO Stmt
parseFile filename =
    ExceptT $
        TextIO.readFile filename
            >>= return . parse (pStmt <* eof) filename

printStore :: ResultT IO Store -> IO ()
printStore maybeStore = runExceptT maybeStore >>= print

parseAndShowFile :: FilePath -> IO ()
parseAndShowFile filename =
    let parsedFile = parseFile filename in
    runExceptT parsedFile >>= print

evalFile :: FilePath -> IO ()
evalFile filename =
    let parsedFile = parseFile filename in
    let evaledStmt = (printStore . (evalStmt Map.empty)) <$> parsedFile in
    runExceptT evaledStmt >>=
        fromRight (putStrLn "Failed to parse")

{- Statement parsers -}

-- The top level parser for statements
pStmt :: Parser Stmt
pStmt = pSequence
  <?> "statement"

-- A simple statement is any statement other than a sequence
pSimpleStmt :: Parser Stmt
pSimpleStmt = lexeme $ choice
    [ try pSkip
    , try pPrint
    , try pAssert
    , try pAssign
    , try pIf
    , try pWhile ]

-- A sequence is one or more statements separated by ';'
pSequence :: Parser Stmt
pSequence =
  Sequence <$> (sepBy1 (sc >> pSimpleStmt) ";" <?> "sequence")

pSkip :: Parser Stmt
pSkip = Skip <$ (lexeme $ string "skip")

pAssign :: Parser Stmt
pAssign =
  do { id <- pRawIdentifier
     ; lexeme $ string ":="
     ; e <- pAexp
     ; return $ Assign id e
  } <?> "assignment"

pIf :: Parser Stmt
pIf =
  do { lexeme $ string "if"
     ; b <- lexeme $ parens pBexp
     ; s1 <- lexeme $ braces pStmt
     ; lexeme $ string "else"
     ; s2 <- lexeme $ braces pStmt
     ; return $ If b s1 s2
  } <?> "if statement"

pWhile :: Parser Stmt
pWhile =
  do { lexeme $ string "while"
     ; b <- lexeme $ parens pBexp
     ; s <- lexeme $ braces pStmt
     ; return $ While b s
  } <?> "while statement"

pPrint :: Parser Stmt
  -- The print syntax is a little wonky right now
  -- TODO: Make this less wonky
pPrint =
  do { lexeme $ string "print"
     ; s <- stringLiteral
     ; a <- pAexp
     ; return $ Print s a
  } <?> "print statement"

pAssert :: Parser Stmt
pAssert =
  do { lexeme $ string "assert"
     ; lexeme $ string "("
     ; b <- pBexp
     ; lexeme $ string ","
     ; s <- stringLiteral
     ; lexeme $ string ")"
     ; return $ Assert b s
  } <?> "assertion"

{- Arithmetic expression parsers -}

pArithTerm :: Parser Aexp
pArithTerm = choice
  [ parens pAexp
  , pIdentifier
  , pInt ]

pAexp :: Parser Aexp
pAexp = makeExprParser pArithTerm arithOperatorTable
  <?> "arithmetic expression"

arithOperatorTable :: [[Operator Parser Aexp]]
arithOperatorTable =
  [ [ prefix "-" (ArithUnaryOp Negate)
    , prefix "+" (ArithUnaryOp Identity) ]
  , [ binary "*" (ArithBinOp Times)
    , binary "/" (ArithBinOp Div)
    , binary "%" (ArithBinOp Mod) ]
  , [ binary "+" (ArithBinOp Plus)
    , binary "-" (ArithBinOp Minus) ]
  ]

{- Helper functions -}

-- Space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

{- Parsers for arithmetic expressions -}

pInt :: Parser Aexp
pInt = Int <$> integer <?> "int"

pRawIdentifier :: Parser String
pRawIdentifier = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

pIdentifier :: Parser Aexp
pIdentifier = Var <$> pRawIdentifier

stringLiteral :: Parser String
stringLiteral = lexeme ((char '"' >> manyTill L.charLiteral (char '"')))

{- Parsers for boolean expressions -}

pBexp :: Parser Bexp
pBexp = makeExprParser pBoolTerm boolOperatorTable
  <?> "boolean expression"

pBoolTerm :: Parser Bexp
pBoolTerm = choice
  [ parens pBexp
  , Boolean True  <$ (lexeme $ string "True")
  , Boolean False <$ (lexeme $ string "False")
  , pArithComparison ]

boolOperatorTable :: [[Operator Parser Bexp]]
boolOperatorTable =
  [ [ prefix "!" (BoolUnaryOp Not) ]
  , [ binary "&&" (BoolBinOp And) ]
  , [ binary "||" (BoolBinOp Or) ]
  , [ binary "==" (BoolBinOp BoolEqual)
    , binary "!=" (BoolBinOp BoolNotEqual) ]
  ]

pArithComparison :: Parser Bexp
pArithComparison =
  do a1 <- pAexp
     op <- pBoolComparator
     a2 <- pAexp
     return $ Comparison op a1 a2

pBoolComparator :: Parser Comparator
pBoolComparator = (lexeme $ choice
  [ Less          <$ string "<"
  , LessEq        <$ string "<="
  , Greater       <$ string ">"
  , GreaterEq     <$ string ">="
  , ArithEqual    <$ string "=="
  , ArithNotEqual <$ string "!=" ])
  <?> "comparator"

{- Helpers for sandwiched expressions -}

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

{- Misc helper functions -}

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

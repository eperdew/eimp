{-# LANGUAGE OverloadedStrings #-}

module Parse
( pAexp
, pBexp
, pStmt
, printStore
, parseFile
) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Text (Text)
import Data.Text.IO as TextIO
import Data.Void
import Data.Either

type Parser = Parsec Void Text

{- Helper functions -}

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

pA_Int :: Parser Aexp
pA_Int = A_Int <$> integer <?> "int"

pIdentifier :: Parser String
pIdentifier = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

pA_Identifier :: Parser Aexp
pA_Identifier = A_Var <$> pIdentifier

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

stringLiteral :: Parser String
stringLiteral = lexeme ((char '"' >> manyTill L.charLiteral (char '"')))

pA_Term :: Parser Aexp
pA_Term = choice
  [ parens pAexp
  , pA_Identifier
  , pA_Int ]

pAexp :: Parser Aexp
pAexp = makeExprParser pA_Term arithOperatorTable
  <?> "arithmetic expression"

arithOperatorTable :: [[Operator Parser Aexp]]
arithOperatorTable =
  [ [ prefix "-" (A_UnaryOp A_Negate) ]
  , [ binary "*" (A_BinOp A_Times) ]
  , [ binary "+" (A_BinOp A_Plus)
    , binary "-" (A_BinOp A_Minus) ]
  ]

pB_Term :: Parser Bexp
pB_Term = choice
  [ parens pBexp
  , B_Boolean True  <$ (lexeme $ string "True")
  , B_Boolean False <$ (lexeme $ string "False")
  , pArithComparison ]

pBexp = makeExprParser pB_Term boolOperatorTable
  <?> "boolean expression"

boolOperatorTable :: [[Operator Parser Bexp]]
boolOperatorTable =
  [ [ prefix "!" (B_UnaryOp B_Not) ]
  , [ binary "&&" (B_BinOp B_And) ]
  , [ binary "||" (B_BinOp B_Or) ]
  , [ binary "==" (B_BinOp B_BoolEqual)
    , binary "!=" (B_BinOp B_BoolNotEqual) ]
  ]

pArithComparison :: Parser Bexp
pArithComparison =
  do a1 <- pAexp
     op <- pB_Comparator
     a2 <- pAexp
     return $ B_Comparison op a1 a2

pB_Comparator :: Parser B_Comparator
pB_Comparator = (lexeme $ choice
  [ B_Less          <$ string "<"
  , B_LessEq        <$ string "<="
  , B_Greater       <$ string ">"
  , B_GreaterEq     <$ string ">="
  , B_ArithEqual    <$ string "=="
  , B_ArithNotEqual <$ string "!=" ])
  <?> "comparator"

pSkip :: Parser Stmt
pSkip = S_Skip <$ (lexeme $ string "skip")

pAssign :: Parser Stmt
pAssign =
  do { id <- pIdentifier
     ; lexeme $ string ":="
     ; e <- pAexp
     ; return $ S_Assign id e
  } <?> "assignment"

pIf :: Parser Stmt
pIf =
  do { lexeme $ string "if"
     ; b <- lexeme $ parens pBexp
     ; s1 <- lexeme $ braces pStmt
     ; lexeme $ string "else"
     ; s2 <- lexeme $ braces pStmt
     ; return $ S_If b s1 s2
  } <?> "if statement"

pWhile :: Parser Stmt
pWhile =
  do { lexeme $ string "while"
     ; b <- lexeme $ parens pBexp
     ; s <- lexeme $ braces pStmt
     ; return $ S_While b s
  } <?> "while statement"

pPrint :: Parser Stmt
pPrint =
  do { lexeme $ string "print"
     ; s <- stringLiteral
     ; a <- pAexp
     ; return $ S_Print s a
  } <?> "print statement"

pAssert :: Parser Stmt
pAssert =
  do { lexeme $ string "assert"
     ; lexeme $ string "("
     ; b <- pBexp
     ; lexeme $ string ","
     ; s <- stringLiteral
     ; lexeme $ string ")"
     ; return $ S_Assert b s
  } <?> "assertion"


pSequence :: Parser Stmt
pSequence =
  S_Sequence <$> (sepBy1 (sc >> pSimpleStmt) ";" <?> "sequence")

pSimpleStmt :: Parser Stmt
pSimpleStmt = lexeme $ choice
  [ try pSkip
  , try pPrint
  , try pAssert
  , try pAssign
  , try pIf
  , try pWhile ]

pStmt :: Parser Stmt
pStmt = pSequence
  <?> "statement"

{- Misc helper functions -}

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

type StmtParseError = ParseErrorBundle Text Void

parseFile :: FilePath -> ExceptT StmtParseError IO Stmt
parseFile filename =
    ExceptT $
        TextIO.readFile filename
            >>= return . parse (pStmt <* eof) filename

printStore :: MaybeT IO Store -> IO ()
printStore maybeStore = runMaybeT maybeStore >>= print

parseAndShowFile :: FilePath -> IO ()
parseAndShowFile filename =
    let parsedFile = parseFile filename in
    runExceptT parsedFile >>= print

evalFile :: FilePath -> IO ()
evalFile filename =
    let parsedFile = parseFile filename in
    let evaledStmt = (printStore . evalStmt) <$> parsedFile in
    runExceptT evaledStmt >>= fromRight (return ())

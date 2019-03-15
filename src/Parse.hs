{-# LANGUAGE OverloadedStrings #-}

module Parse
( pRvalue
, pStmt
, parseFile
, parseTestFile
, parseAndShowFile
, parseAndTypecheckFile
, evalFile
) where

import Ast
import Types
import Eval
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

parseTestFile :: FilePath -> IO ()
parseTestFile filename = TextIO.readFile filename >>= parseTest pFile

parseFile :: FilePath -> ExceptT StmtParseError IO (Stmt ())
parseFile filename =
    ExceptT $
        TextIO.readFile filename
            >>= return . parse pFile filename

printContext :: ResultT IO TContext -> IO ()
printContext maybeCtx = runExceptT maybeCtx >>= print

parseAndShowFile :: FilePath -> IO ()
parseAndShowFile filename =
    let parsedFile = parseFile filename in
    runExceptT parsedFile >>= print

parseAndTypecheckFile :: FilePath -> IO ()
parseAndTypecheckFile filename =
    let parsedFile = parseFile filename in
    let typecheckedStmt = typecheckTStmt Map.empty <$> parsedFile in
    join (print <$> runExceptT typecheckedStmt)

evalFile :: FilePath -> IO ()
evalFile filename =
    let parsedFileIO = runExceptT $ parseFile filename in
    parsedFileIO >>= \parsedFile -> case parsedFile of
      Left error -> putStrLn "Failed to parse"
      Right stmt ->
        let typedResult = typecheckTStmt Map.empty stmt in
        case typedResult of
          Left error -> print error
          Right (typedStmt, _) ->
            let evalResult = evalTStmt Map.empty typedStmt in
            (runExceptT evalResult) >>= print

pFile :: Parser (Stmt ())
pFile = sc *> pStmt <* eof

{- Statement parsers -}

-- The top level parser for statements
pStmt :: Parser (Stmt ())
pStmt = pSequence
  <?> "statement"

-- A simple statement is any statement other than a sequence
pSimpleStmt :: Parser (Stmt ())
pSimpleStmt = lexeme $ choice
    [ try pSkip
    , try pPrint
    , try pAssert
    , try pDeclaration
    , try pAssign
    , try pIf
    , try pWhile ]

-- A sequence is one or more statements separated by ';'
pSequence :: Parser (Stmt ())
pSequence =
  Sequence () <$> some pSimpleStmt <?> "sequence"

pSkip :: Parser (Stmt ())
pSkip = do { symbol "skip"
           ; symbol ";"
           ; return $ Skip () } <?> "skip"

pDeclaration :: Parser (Stmt ())
pDeclaration =
  do { id <- pRawIdentifier
     ; symbol ":"
     ; t <- pType
     ; symbol ";"
     ; return $ Declaration () id t
  } <?> "declaration"

pType :: Parser Type
pType = lexeme $ choice
  [ IntegerT <$ symbol "int"
  , BooleanT <$ symbol "bool" ]

pAssign :: Parser (Stmt ())
pAssign =
  do { id <- pRawIdentifier
     ; symbol ":="
     ; e <- pRvalue
     ; symbol ";"
     ; return $ Assign () id e
  } <?> "assignment"

pIf :: Parser (Stmt ())
pIf =
  do { symbol "if"
     ; b <- lexeme $ parens pRvalue
     ; s1 <- lexeme $ braces pStmt
     ; symbol "else"
     ; s2 <- lexeme $ braces pStmt
     ; return $ If () b s1 s2
  } <?> "if statement"

pWhile :: Parser (Stmt ())
pWhile =
  do { symbol "while"
     ; b <- lexeme $ parens pRvalue
     ; s <- lexeme $ braces pStmt
     ; return $ While () b s
  } <?> "while statement"

pPrint :: Parser (Stmt ())
  -- The print syntax is a little wonky right now
  -- TODO: Make this less wonky
pPrint =
  do { symbol "print"
     ; s <- stringLiteral
     ; a <- pRvalue
     ; symbol ";"
     ; return $ Print () s a
  } <?> "print statement"

pAssert :: Parser (Stmt ())
pAssert =
  do { symbol "assert"
     ; symbol "("
     ; b <- pRvalue
     ; symbol ","
     ; s <- stringLiteral
     ; symbol ")"
     ; symbol ";"
     ; return $ Assert () b s
  } <?> "assertion"

{- Arithmetic expression parsers -}

pRvalue :: Parser (Rvalue ())
pRvalue = lexeme (makeExprParser pRvalueTerm rvalueOperatorTable)
  <?> "rvalue"

pRvalueTerm :: Parser (Rvalue ())
pRvalueTerm = choice
  [ pInt
  , pBool
  , pVar
  , parens pRvalue ]

pInt :: Parser (Rvalue ())
pInt = Int () <$> integer <?> "int"

pBool :: Parser (Rvalue ())
pBool = choice
  [ Boolean () True  <$ symbol "True"
  , Boolean () False <$ symbol "False" ]

pVar :: Parser (Rvalue ())
pVar = Var () <$> pRawIdentifier

rvalueOperatorTable :: [[Operator Parser (Rvalue ())]]
rvalueOperatorTable =
  [ [ prefix "-" (Unop () UnaryMinus)
    , prefix "+" (Unop () UnaryPlus)
    , prefix "!" (Unop () Not) ]
  , [ binary "*" (Binop () Times)
    , binary "/" (Binop () Div)
    , binary "%" (Binop () Mod) ]
  , [ binary "+" (Binop () Plus)
    , binary "-" (Binop () Minus) ]
  , [ binary "==" (Binop () Equal)
    , binary "!=" (Binop () NotEqual)
    , binary "<"  (Binop () Less)
    , binary "<=" (Binop () LessEq)
    , binary ">"  (Binop () Greater)
    , binary ">=" (Binop () GreaterEq) ]
  , [ binary "&&" (Binop () And) ]
  , [ binary "||" (Binop () Or) ] ]

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

pRawIdentifier :: Parser String
pRawIdentifier = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

stringLiteral :: Parser String
stringLiteral = lexeme ((char '"' >> manyTill L.charLiteral (char '"')))

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

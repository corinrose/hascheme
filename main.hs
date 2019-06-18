module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main = do getArgs >>= print . eval . readExpr . head

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

{- Display -}

unwordsList = unwords . map showVal

showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

{- Evaluation -}

primitives = [  ("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quotient", numericBinop quot),
                ("remainder", numericBinop rem),
                ("symbol?", isSymbol),
                ("string?", isString),
                ("number?", isNumber)]


eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(List _) = val

apply func args = maybe (Bool False) ($ args) $ lookup func primitives

numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                        if null parsed then 0
                        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

isSymbol ((Atom _):_) = Bool True
isSymbol _ = Bool False

isString ((String _):_) = Bool True
isStrin _ = Bool False

isNumber ((Number _):_) = Bool True
isNumber _ = Bool False

{- Parsing -}

symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do  char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber = many1 digit >>= (return . Number . read)

parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail 

parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

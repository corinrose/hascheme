module Main where
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main = do 
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled

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

{- Exceptions -}

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected: " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue (Right val) = val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) =  mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
    ($ args)
    (lookup func primitives)

numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@([_]) = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                        if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isSymbol ((Atom _):_) = return $ Bool True
isSymbol _ = return $ Bool False

isString ((String _):_) = return $ Bool True
isString _ = return $ Bool False

isNumber ((Number _):_) = return $ Bool True
isNumber _ = return $ Bool False

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

module Main where

import Control.Monad (void)
import Data.Char (toUpper)
import Data.List (intersperse, isSuffixOf)
import Text.Parsec
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

----------------------------------------------------------------

lexer :: P.TokenParser a
lexer = P.makeTokenParser javaStyle {
    P.reservedNames = ["enum", "struct", "select", "case"]
  }

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

angles :: Parser a -> Parser a
angles = P.angles lexer

identifier :: Parser String
identifier  = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reserved lexer

natural :: Parser Int
natural = fromIntegral <$> (try (P.hexadecimal lexer) <|> P.natural lexer)

semi :: Parser ()
semi = void $ P.semi lexer

comma :: Parser ()
comma = void $ P.comma lexer

colon :: Parser ()
colon = void $ P.colon lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> Parser ()
symbol = void . P.symbol lexer

----------------------------------------------------------------

type NAME = String --TYPENAME or DEFNAME

type TYPENAME = String
data TYPE = TYPE NAME TYPENAME AMOUNT deriving Show

data AMOUNT = SCALAR
            | VECTOR  Int     -- Int = byte count
            | VVECTOR Int Int -- min .. max
            deriving Show

type DEFNAME = String
data DEF = ENUM   DEFNAME Int [ENUMITEM] -- Int = max value
         | STRUCT DEFNAME [MEMBER]
         | ALIAS  TYPE
         deriving Show

type ENUMNAME = String
data ENUMITEM = ENUMITEM ENUMNAME ENUMVALUE deriving Show
data ENUMVALUE = SOLO  Int
               | RANGE Int Int
               deriving Show

type MEMNAME = String
type SELECTOR = String
data MEMBER = FIXED  TYPE
            | SELECT (Maybe MEMNAME) SELECTOR [CASE]
            deriving Show

type CASENAME = String
data CASE = CASE CASENAME NAMEORTYPE deriving Show
data NAMEORTYPE = CASE1 TYPENAME
                | CASE2 TYPE
                deriving Show

----------------------------------------------------------------

enumItem :: Parser ENUMITEM
enumItem = ENUMITEM <$> identifier <*> parens enumValue

enumValue :: Parser ENUMVALUE
enumValue = do
    beg <- natural
    option (SOLO beg) $ do
        symbol ".."
        end <- natural
        return $ RANGE beg end

enum :: Parser DEF
enum = do
    reserved "enum"
    (lst,mx) <- braces $ do
        el <- endBy1 enumItem comma
        n <- parens natural
        return (el,n)
    name <- identifier
    semi
    return $ ENUM name mx lst

struct :: Parser DEF
struct = do
    reserved "struct"
    lst <- braces $ endBy smember semi
    name <- identifier
    semi
    return $ STRUCT name lst

smember :: Parser MEMBER
smember = fixed <|> select

fixed :: Parser MEMBER
fixed = FIXED <$> type'

type' :: Parser TYPE
type' = do
    basetyp <- identifier
    name <- identifier
    amount <- option SCALAR (vec <|> vvec <|> assign)
    return $ TYPE name basetyp amount
  where
    vec = VECTOR <$> brackets natorsym
    natorsym = try natural <|> (many1 (noneOf "]") >> return (-1))
    vvec = angles $ do
        beg <- natural
        symbol ".."
        end <- arith
        return $ VVECTOR beg end
    arith = do
        x <- natural
        option x $ do
            symbol "^"
            y <- natural
            option (x ^ y) $ do
                symbol "-"
                z <- natural
                return $! x ^ y - z
    assign = do
        symbol "="
        void $ braces $ sepBy natural comma
        return SCALAR

select :: Parser MEMBER
select = do
    reserved "select"
    sel <- parens selector
    cases <- braces $ endBy1 caseItem semi
    mname <- optionMaybe identifier
    return $ SELECT mname sel cases
  where
    selector = do
        p <- identifier
        option p $ do
            symbol "."
            c <- identifier
            return $! p ++ "." ++ c
    caseItem = do
        reserved "case"
        name <- identifier
        colon
        typ <- try (CASE2 <$> type') <|> (CASE1 <$> identifier)
        return $ CASE name typ

alias :: Parser DEF
alias = do
    typ <- type'
    semi
    return $ ALIAS typ

defs :: Parser [DEF]
defs = many (enum <|> struct <|> alias)

tls13 :: Parser [DEF]
tls13 = do
    whiteSpace
    defs

----------------------------------------------------------------

up :: String -> String
up = map toUpper

pp :: DEF -> IO ()
pp (ENUM nm n xs) = do
    putStr "data "
    putStr $ up nm
    putStr " = "
    putStrLn $ concat $ intersperse " | " $ map up $ filter (\x -> not ("_RESERVED" `isSuffixOf` x)) $ map extract xs
pp (ALIAS (TYPE new old _)) = do
    let old' = up old
    putStrLn $ "newtype " ++ new ++ " = " ++ new ++ " " ++ old'
pp (STRUCT nm _)  = do
    putStrLn $ "data " ++ nm ++ " = " ++ nm ++ " {"
    putStrLn "  }"

extract :: ENUMITEM -> ENUMNAME
extract (ENUMITEM nm _) = nm

----------------------------------------------------------------

main :: IO ()
main = do
    ex <- parse tls13 "tls13" <$> getContents
    case ex of
      Left err -> print err
      Right  x -> do
          putStrLn "import Data.Word"
          putStrLn "type UINT8 = Word8"
          putStrLn "type OPAQUE = ()"
          mapM_ pp x

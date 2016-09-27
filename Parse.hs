module Main where

import Control.Monad (void)
import Data.Char (toUpper, toLower, isLower)
import Data.List (intersperse, isSuffixOf)
import System.Random
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
data ENUMITEM = ENUMITEM {
    enumname  :: ENUMNAME
  , enumvalue :: ENUMVALUE
  } deriving Show
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

down :: String -> String
down = map toLower

up :: String -> String
up = map toUpper

cap :: String -> String
cap "" = ""
cap (c:cs) = toUpper c : cs

toTypeName :: String -> String
toTypeName "" = ""
toTypeName (c:cs) = toUpper c : toTypeName' cs

toTypeName' :: String -> String
toTypeName' "" = ""
toTypeName' ('_':cs) = toTypeName cs
toTypeName' (c:cs) = c : toTypeName' cs

nonReserved :: String -> Bool
nonReserved x = not ("_RESERVED" `isSuffixOf` x)

pp :: DEF -> IO ()
pp (ENUM nm _n xs) = do
    putStr "data "
    putStr nm
    putStr " =\n      "
    let xs' = intersperse "\n    | " $ map up $ filter nonReserved $ map enumname xs
    mapM_ putStr xs'
    putStr "\n    deriving (Show,Eq,Ord,Enum,Bounded)\n"
pp (ALIAS (TYPE new old _)) = do
    let old' = cap old
    putStr "newtype "
    putStr new
    putStr " = "
    putStr new
    putStr " "
    putStr old'
    putStr "\n"
pp (STRUCT nm ms)  = do
    putStr "data "
    putStr nm
    putStr " = "
    putStr nm
    putStr " {\n"
    cs <- ppMembers nm ms
    putStr "  }\n"
    ppCases cs

ppMembers :: String -> [MEMBER] -> IO [CASE]
ppMembers _ [] = return []
ppMembers nm (m:ms) = do
    let nm' = down nm
    cs <- ppMember ("    _" ++ nm' ++ "_") m
    css <- mapM (ppMember ("  , _" ++ nm' ++ "_")) ms
    let cases = concat (cs:css)
    return cases

ppMember :: String -> MEMBER -> IO [CASE]
ppMember pre (FIXED (TYPE field typ _)) = ppField pre field typ >> return []
ppMember _   (SELECT _ _ cs) = return cs

ppCases :: [CASE] -> IO ()
ppCases [] = return ()
ppCases cs = do
  r <- randomIO :: IO Int
  let a = "_a" ++ show (abs r)
  putStr a
  putStrLn " :: ()"
  putStr a
  putStrLn " = case undefined of"
  mapM_ ppCase cs
  putStrLn "    _ -> ()"

ppCase :: CASE -> IO ()
ppCase (CASE nm c) = do
    putStr "    "
    putStr (up nm)
    putStr " -> seq "
    let typ = caseName c
    if isLower (head typ) || (typ == "NamedGroup") then do -- FIXME
        putStr "(undefined :: "
        putStr typ
        putStrLn ") ()"
      else do
        putStr typ
        putStrLn "{} ()"

caseName :: NAMEORTYPE -> TYPENAME
caseName (CASE1 typ) = typ
caseName (CASE2 (TYPE _ typ _)) = typ

ppField :: String -> String -> String -> IO ()
ppField pre field typ = do
    putStr pre
    putStr field
    putStr " :: "
    putStr $ cap typ
    putStr "\n"

----------------------------------------------------------------

main :: IO ()
main = do
    ex <- parse tls13 "tls13" <$> getContents
    case ex of
      Left err -> print err
      Right  x -> do
          putStrLn "module TLS13 (TLSPlaintext(..), TLSInnerPlaintext(..), TLSCiphertext(..)) where"
          putStrLn "import Data.Word"
          putStrLn "type Uint8 = Word8"
          putStrLn "type Uint16 = Word16"
          putStrLn "type Uint24 = Word"
          putStrLn "type Uint32 = Word32"
          putStrLn "type Opaque = ()"
          mapM_ pp x

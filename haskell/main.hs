import Data.Char
import Data.List
import Data.List.Split

data TokenType = SEP | VAL | OP | NUM | FUNC | VAR | ERROR deriving (Eq, Show, Ord)
data Token = Token {text :: [Char], type_t :: TokenType} deriving (Eq, Ord)

instance Show Token where
    show (Token text type_t) = "('" ++ text ++ "', " ++ (show type_t) ++ ")"

createToken :: [Char] -> [Token]
createToken xs = if elem (head xs) "()" then [Token xs SEP] else
                 if elem (head xs) "^+-*/=" then [Token xs OP] else
                 if all (\c -> isDigit c || c == '.') xs then [Token xs NUM] else
                 if length xs == 1 then [Token xs VAR] else
                 if isDigit (head xs) then
                    let i = (length . filter (\c -> isDigit c || c == '.')) xs in
                        let (n, o) = splitAt i xs in
                            [Token n NUM, (Token o (if length o == 1 then VAR else FUNC))]
                 else [Token xs FUNC]

splitEq :: [Char] -> [Token]
splitEq xs = concat [createToken x | x <- [x | x <- split (oneOf "( )^+-*/=") xs, x /= " " && x /= ""]]

tokensToTree xs = xs

parse = tokensToTree . splitEq

main = do
        print $ parse "sin(x) * (x^2 + 4.2x(10 - 34x)) + 5.3 = 45"

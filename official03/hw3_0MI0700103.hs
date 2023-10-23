import Text.Parsec
import Text.Parsec.String
import Data.List

main :: IO()
main = do

  print $ simplify "1+2+x"  == "x+3"
  print $ simplify "x+2+x-2" == "2x"
  print $ simplify "x+2-(x-2)" == "4"
  print $ simplify "y+2+x-2" == "x+y"
  print $ simplify "1+2+x+y+x+z+5-x-x-x+y" == "-x+2y+z+8"
  print $ simplify "1+2+x+y+x-(x-x-x)+z+y-9" == "3x+2y+z-6"
  print $ simplify "1+2-(3-(3-2))-9" == "-8"
  print $ prune (T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]]) == T 1 [T 2 [T 3 []],T 4 [T 5 []],T 7 [T 8 [],T 9 [T 10 []]]]


data NTree a = T a [NTree a]
  deriving (Eq, Show)

prune ::  NTree a -> NTree a
prune t@(T value children)
  | isStick t = T value (map (\ (T y _) -> (T y [])) children)
  | otherwise = T value (map prune children)
   where
    isStick (T _ []) = True
    isStick (T _ [children]) = isStick children
    isStick _ = False


simplifyExpression :: Expression -> String
simplifyExpression (Constant num) = show num
simplifyExpression (Variable var) = [var]
simplifyExpression (Add expr1 expr2) = simplifyExpression expr1 ++ "+" ++ simplifyExpression expr2
simplifyExpression (Subtract expr1 expr2) = simplifyExpression expr1 ++ "-" ++ simplifyExpression expr2
simplifyExpression (Multiply expr1 expr2) = simplifyFactor expr1 ++ "*" ++ simplifyFactor expr2
simplifyExpression (Divide expr1 expr2) = simplifyFactor expr1 ++ "/" ++ simplifyFactor expr2

simplifyFactor :: Expression -> String
simplifyFactor (Constant num) = show num
simplifyFactor (Variable var) = [var]
simplifyFactor expr@(Add _ _) = "(" ++ simplifyExpression expr ++ ")"
simplifyFactor expr@(Subtract _ _) = "(" ++ simplifyExpression expr ++ ")"
simplifyFactor expr = simplifyExpression expr

simplify :: String -> String
simplify input = sort $ simplifyExpression (parseExpr input)

data Expression = Constant Int
                | Variable Char
                | Add Expression Expression
                | Subtract Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                deriving Show

expression :: Parser Expression
expression = chainl1 term additionSubtraction

term :: Parser Expression
term = chainl1 factor multiplicationDivision

factor :: Parser Expression
factor = constant <|> variable <|> parentheses

constant :: Parser Expression
constant = Constant . read <$> many1 digit

variable :: Parser Expression
variable = Variable <$> oneOf ['a'..'z']

parentheses :: Parser Expression
parentheses = char '(' *> expression <* char ')'

additionSubtraction :: Parser (Expression -> Expression -> Expression)
additionSubtraction = addOp "+" Add <|> addOp "-" Subtract
  where addOp opString constructor = reservedOp opString >> return constructor

multiplicationDivision :: Parser (Expression -> Expression -> Expression)
multiplicationDivision = mulOp "*" Multiply <|> mulOp "/" Divide
  where mulOp opString constructor = reservedOp opString >> return constructor


parseExpr :: String -> Expression
parseExpr input = case parse expression "" input of
    Left err -> error $ "Parsing error: " ++ show err
    Right expr -> expr

reservedOp :: String -> Parser ()
reservedOp opString = spaces >> string opString >> spaces >> return ()
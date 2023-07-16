module Parser where

import Text.Parsec
import Linsys

type Parser = Parsec String ()

equality :: Parser Equation
equality = spaces *> do
    fst <- linear_expr
    spaces
    char '='
    spaces
    snd <- linear_expr
    spaces
    char ';'
    spaces
    return $ subEquation fst snd

linear_expr :: Parser Equation
linear_expr = spaces *> (term `chainl1` (addop <|> subop)) <* spaces

term :: Parser Equation
term = spaces *> (factor `chainl1` (mulop <|> divop))

factor :: Parser Equation
factor = spaces *> (variable <|> number <|> between (char '(') (char ')') linear_expr)

addop = spaces *> char '+' *> spaces *> return addEquation
subop = spaces *> char '-' *> spaces *> return subEquation
mulop = spaces *> char '*' *> spaces *> return mulEquation
divop = spaces *> char '/' *> spaces *> return divEquation

addEquation :: Equation -> Equation -> Equation
addEquation (Equation a_terms a_constant) (Equation b_terms b_constant) =
    Equation (a_terms ++ b_terms) (a_constant + b_constant)

negateEquation :: Equation -> Equation
negateEquation (Equation terms constant) = (Equation (fmap (\(qt, vn) -> (-qt, vn)) terms) (-constant))

subEquation :: Equation -> Equation -> Equation
subEquation a b = addEquation a (negateEquation b)

mulEquation :: Equation -> Equation -> Equation
mulEquation (Equation terms const) (Equation [] const') = Equation (fmap (\(qt, vn) -> (qt * const', vn)) terms) (const * const')
mulEquation (Equation [] const') (Equation terms const)= Equation (fmap (\(qt, vn) -> (qt * const', vn)) terms) (const * const')
mulEquation _ _ = error "The resulting system is non-linear"

divEquation :: Equation -> Equation -> Equation
divEquation (Equation terms const) (Equation [] const') = Equation (fmap (\(qt, vn) -> (qt / const', vn)) terms) (const / const')
divEquation (Equation [] const') (Equation terms const)= Equation (fmap (\(qt, vn) -> (qt / const, vn)) terms) (const' / const)
divEquation _ _ = error "The resulting system is non-linear"

number :: Parser Equation
number = spaces *> do
    sign <- ((char '-' *> return (* (-1))) <|> return id)
    fst <- read <$> many1 digit <|> return 0
    dec <- decimals <|> return 0
    return $ Equation [] (sign (fst + dec))

decimals :: Parser Double
decimals = do
    char '.'
    digits <- many1 digit
    return $ (foldr1 (\x y -> x + y / 10) $ fmap (read . (:[])) digits) / 10

variable :: Parser Equation
variable = spaces *> do
    char '$'
    fst <- letter
    rest <- many (alphaNum <|> char '_')
    let varname = fst:rest
    return $ Equation [(1, Variable varname)] 0

system :: Parser [Equation]
system = many (equality) <* eof

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Linsys where

import Data.List ( find, groupBy, nub, sortBy )


newtype Variable = Variable String
    deriving (Eq, Ord)

data Equation = Equation [(Double, Variable)] Double
    deriving (Eq)

instance Show Variable where
    show (Variable var) = '$' : var

instance Show Equation where
    show (Equation terms constant) = showEq' $ ((\(qt, Variable var) -> (qt, var)) <$> terms) ++ [(constant, "")] where
        showEq' [] = ""
        showEq' [(qt, var)] = show qt ++ var
        showEq' ((qt1, var1) : term2@(qt2, _) : rest)
            | qt1 == 0 = showEq' (term2 : rest)
            | qt2 >= 0 = show qt1 ++ var1 ++ "+" ++ showEq' (term2 : rest)
            | qt2 < 0 = show qt1 ++ var1 ++ showEq' (term2 : rest)
            | otherwise = error "This should never happen"

gauss :: [[Double]] -> [[Double]]
gauss mx = gauss' mx 0 0

gauss' :: [[Double]] -> Int -> Int -> [[Double]]
gauss' mx i j
    | i == length mx = mx
    | j == length (head mx) = mx
    | otherwise = case find (\row -> mx !! row !! j /= 0) [i..length mx-1] of
        Nothing -> gauss' mx i (j+1)
        Just i' ->
            let mx1 = swapRows mx i i' in
            let mx2 = (\r -> if r == i
                then (*) (1 / mx1 !! i !! j) <$> mx1 !! r
                else addRows (mx1 !! r) ((*) (-mx1 !! r !! j / mx1 !! i !! j) <$> (mx1 !! i))) <$> [0..length mx-1] in
            gauss' mx2 (i+1) (j+1)
    where
        swapRows :: [[Double]] -> Int -> Int -> [[Double]]
        swapRows mx i j = (\(row, pos) -> if pos == i then mx !! j else if pos == j then mx !! i else row ) <$> zip mx [0..]

        addRows :: [Double] -> [Double] -> [Double]
        addRows = zipWith (+)


reduce :: Equation -> Equation
reduce (Equation terms0 constant) = Equation ans constant where
    terms = sortBy (\(_, v1) (_, v2) -> compare v1 v2) terms0
    ans :: [(Double, Variable)]
    ans =  (\l@(x:rest) -> (sum $ fst <$> l, snd x))
       <$> groupBy (\(_, v1) (_, v2) -> v1 == v2) terms

solve :: [Equation] -> Maybe [(Variable, Equation)]
solve eqs0 = if consistent then Just ans else Nothing where
    eqs = reduce <$> eqs0
    vars = nub $ do
        Equation terms _ <- eqs;
        snd <$> terms

    colOf v = snd
            $ head
            $ filter (\(v', _) -> v == v')
            $ zip vars [0..]

    n = length eqs
    m = length vars

    matrix = do
        i <- [0..n-1];
        let terms = do {
            j <- [0..m-1];
            return $ case find (\(_, v) -> v == vars !! j) (let Equation eqs' _ = eqs !! i in eqs') of
                Nothing -> 0
                Just (c, _) -> c
        };
        return $ terms ++ [let Equation _ c = eqs !! i in c]

    solvedMatrix = gauss matrix

    consistent = not $ any (\row -> all (==0) (init row) && last row /= 0) solvedMatrix

    ans = do
        v <- vars
        let col = colOf v
        let bound = flip any [0..n-1] $ \i -> solvedMatrix !! i !! col /= 0 && all (\j -> solvedMatrix !! i !! j == 0) [0..col-1] 
        if bound then
            let i = head $ filter (\i -> solvedMatrix !! i !! col /= 0) [0..n-1] in
            let myEq = flip Equation (-solvedMatrix !! i !! m) $ do {
                j <- [0..m-1];
                if j == col then
                    []
                else
                    return (-solvedMatrix !! i !! j, vars !! j)
            }; in
            return (v, myEq)
        else
            return (v, Equation [] 0)


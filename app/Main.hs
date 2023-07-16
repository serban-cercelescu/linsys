module Main (main) where

import Data.List
import System.Environment
import System.IO
import Text.Parsec
import Control.Monad

import qualified Parser
import qualified Linsys
import Writer

answer :: [Linsys.Equation] -> String
answer eqs = case (fmap.fmap) (\(var, eq) -> (show var, eq)) $ Linsys.solve eqs of
    Nothing -> "No solution"
    Just solution -> ans solution
    where
        vars = fmap show $ nub $ concatMap (\(Linsys.Equation terms _) -> snd <$> terms) eqs
        ans solution = snd $ runWriter $ forM_ vars $ \var -> do {
            tell $ var ++ " = " ++ (case find ((==var) . fst) solution of
                Just (_, eq) -> if eq == Linsys.Equation [] 0 then var else show eq
                Nothing -> error "The program crashed for reasons beyond one's comprehension.");
            tell "\n"
        };

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            contents <- getContents
            putStrLn $ case runParser Parser.system () "" contents of
                Left err -> show err
                Right parsed -> answer parsed

        (filename:_) -> do
            contents <- readFile filename
            putStrLn $ case runParser Parser.system () "" contents of
                Left err -> show err
                Right parsed -> answer parsed

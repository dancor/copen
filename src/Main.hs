{-# LANGUAGE TemplateHaskell #-}

import Chess
import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.DeriveTH
import Data.Either
import Data.List
import Data.Maybe
import Data.PGN
import Data.SAN
import Database.HDBC
import Database.HDBC.PostgreSQL
import FUtil
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Parsec
-- wtf?  binary needs this?
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Map as M

data Options = Options {
  optImport :: Bool
}

$(derive makeBinary ''Board)
$(derive makeBinary ''Color)
$(derive makeBinary ''BdSq)
$(derive makeBinary ''Move)
$(derive makeBinary ''Result)

defOpts :: Options
defOpts = Options {
  optImport = False
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "i" ["import"] (NoArg (\ o -> o {optImport = True}))
    "Import .PGN files into copen's database."
  ]

onFiles :: [[Char]] -> ([Char] -> String -> IO a) -> IO ()
onFiles files f = mapM_
  (\ file -> f file =<< if file == "-" then getContents else readFile file)
  files

-- this is a hack for This Week in Chess's .pgn files
removeLineHeaders :: [String] -> [String]
removeLineHeaders (s1:s2:rest) = if "----" `isPrefixOf` s2
  then removeLineHeaders rest else s1 : removeLineHeaders (s2:rest)
removeLineHeaders [s1] = [s1]
removeLineHeaders [] = []

gameBoards :: [Move] -> [(Move, Board)]
gameBoards = gameBoardsBd bdInit where
  gameBoardsBd bd [] = []
  gameBoardsBd bd (mv:mvs) = (mv', bd) : gameBoardsBd (bdDoMv mv' bd) mvs where
    Right mv' = resolveMv bd mv

processGame :: Connection -> String -> Int -> PGN -> IO ()
processGame conn filename gameNum pgn = do
  let
    moves = map fst $ pgnMoves pgn
  unless (null moves) $ do
    gameId <- recordGame conn filename gameNum $ pgnResult pgn
    mapM_ (\ (ply, (mv, bd)) -> recordPosMove conn gameId ply mv bd) .
      zip [1..] $ gameBoards moves

processGames :: Connection -> String -> [(Int, PGN)] -> IO ()
processGames conn filename = mapM_ (uncurry $ processGame conn filename)

-- games_game_number_seq
recordGame :: Connection -> String -> Int -> Result -> IO Int
recordGame conn filename gameNum result = do
  ret <- withTransaction conn $ \ conn -> quickQuery conn
    "INSERT INTO games (file, number_in_file, result) VALUES (?, ?, ?) \
    \RETURNING game_number"
    [toSql filename, toSql gameNum, toSql $ fromEnum result]
  commit conn
  return . fromSql . head $ head ret

recordPosMove :: Connection -> Int -> Int -> Move -> Board -> IO ()
recordPosMove conn gameId ply move board = do
  withTransaction conn $ \ conn -> run conn
    "INSERT INTO copen (position, move, game_number, ply) VALUES (?, ?, ?, ?)"
    [toSql $ encode board, toSql $ encode move, toSql gameId, toSql ply]
  commit conn

getPosMoves :: Connection -> Board -> IO [Move]
getPosMoves conn board = do
  ret <- withTransaction conn $ \ conn -> quickQuery conn
    "SELECT move, game_number, ply FROM copen WHERE position = ?"
    [toSql $ encode board]
  return $ map (decode . fromSql . head) ret

withConn :: (Connection -> IO a) -> IO ()
withConn f = do
  conn <- handleSqlError $ connectPostgreSQL "dbname=copen"
  f conn
  disconnect conn

main :: IO ()
main = do
  let cClean = unlines . removeLineHeaders . lines
  (opts, args) <- doArgs "usage" defOpts options
  args <- getArgs
  withConn $ \ conn -> if optImport opts
    then
      onFiles args $ \ filename c -> do
        putStrLn $ "processing " ++ show filename
        case runParser (many1 pgnParser <* eof) () filename $ cClean c of
          Left err -> error $ show err
          Right pgns -> processGames conn filename $ zip [1..] pgns
        putStrLn $ "finished " ++ show filename
    else do
      mvs <- getPosMoves conn bdInit
      print $ length mvs

{-# LANGUAGE TemplateHaskell #-}

import Chess
import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Data.Binary
import Data.Char
import Data.DeriveTH
import Data.Either
import Data.List
import Data.Maybe
import Data.PGN
import Data.SAN
import Database.PostgreSQL.Enumerator
import FUtil
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Parsec
import qualified Data.ByteString.Lazy as BS
-- why does binary need this?
import qualified Data.ByteString.Lazy.Internal as BSI
import qualified Data.Map as M

data Options = Options {
  optImport :: Bool
}

instance Applicative (DBM mark sess) where
  pure = return
  (<*>) = ap

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

onFiles :: (MonadIO m) => [[Char]] -> ([Char] -> String -> m a) -> m ()
onFiles files f = mapM_
  (\ file -> f file =<< io (if file == "-" then getContents else readFile file))
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

importGame filename gameNum pgn = do
  let
    moves = map fst $ pgnMoves pgn
  unless (null moves) $ do
    gameId <- recordGame filename gameNum $ pgnResult pgn
    mapM_ (\ (ply, (mv, bd)) -> recordPosMove gameId ply mv bd) .
      zip [1 :: Int ..] $ gameBoards moves

importGames filename = mapM_ (uncurry $ importGame filename)

oneIntIteratee :: (Monad m) => Int -> IterAct m Int
oneIntIteratee = const . return . Left

recordGame filename gameNum result = doQuery (sqlbind
  "INSERT INTO games (file, number_in_file, result) VALUES (?, ?, ?) \
  \RETURNING game_number"
  [bindP filename, bindP gameNum, bindP $ fromEnum result])
  oneIntIteratee undefined
  <* commit

recordPosMove gameId ply move board = execDML (cmdbind
  "INSERT INTO copen (position, move, game_number, ply) VALUES (?, ?, ?, ?)"
  [bindP . BS.unpack $ encode board, bindP . BS.unpack $ encode move,
   bindP gameId, bindP ply])
  <* commit

getPosMovesIteratee :: (Monad m) => [Word8] -> Int ->
  IterAct m [(Move, Result)]
getPosMovesIteratee a b accum =
  result' $ ((decode $ BS.pack a, toEnum b):accum)

getPosMoves board = doQuery (sqlbind
  "SELECT copen.move, games.result FROM copen, games WHERE copen.game_number = games.game_number AND copen.position = ?"
  [bindP . BS.unpack $ encode board])
  getPosMovesIteratee []

main :: IO ()
main = do
  let cClean = unlines . removeLineHeaders . lines
  (opts, args) <- doArgs "usage" defOpts options
  withSession (connect [CAdbname "copen"]) $ if optImport opts
    then
      onFiles args $ \ filename c -> do
        io . putStrLn $ "processing " ++ show filename
        case runParser (many1 pgnParser <* eof) () filename $ cClean c of
          Left err -> error $ show err
          Right pgns -> importGames filename $ zip [1 :: Int ..] pgns
        io . putStrLn $ "finished " ++ show filename
    else do
      mvs <- getPosMoves bdInit
      let
        mvsInfo :: M.Map Move (M.Map Result Int)
        mvsInfo = M.fromListWith (M.unionWith (+)) $
          map (second (flip M.singleton 1)) mvs
        sumAndPercents :: M.Map Result Int -> (Int, [Double])
        sumAndPercents rs = (rSum, percents) where
          rSum = sum rsl
          percents = map (\ r -> fromIntegral r / fromIntegral rSum) rsl
          rsl = map snd $ M.toList rs
        res = reverse . sort . map swap . M.toList $
          M.map sumAndPercents mvsInfo
      io $ mapM_ print res

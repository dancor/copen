{-# LANGUAGE TemplateHaskell #-}

import Chess
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Binary
import Data.DeriveTH
--import Data.Digest.OpenSSL.MD5
import Data.Either
import Data.List
import Data.PGN
import Data.SAN
import System.Environment
import System.IO
import Text.Parsec
-- wtf?  binary needs this?
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Map as M

type Hash = BS.ByteString

data GamePosInfo = GamePosInfo {
  gpiFilename :: String,
  gpiGameNum :: Int,
  gpiPlyCount :: Int,
  gpiGameResult :: Result
  }
  deriving (Eq, Show)

type PosHashToInfo = M.Map Hash (M.Map Move [GamePosInfo])

$(derive makeBinary ''Board)
$(derive makeBinary ''Color)
$(derive makeBinary ''BdSq)
$(derive makeBinary ''Move)
$(derive makeBinary ''Result)
$(derive makeBinary ''GamePosInfo)

onFiles :: [String] -> (String -> String -> a) -> IO [a]
onFiles files f = mapM
  (\ file -> f file <$> if file == "-" then getContents else readFile file)
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

processGame :: String -> Int -> PGN -> PosHashToInfo
processGame filename gameNum pgn = if null moves then M.empty else
  M.unionsWith M.union . map f . zip [1..] $ gameBoards moves
  where
  f :: (Int, (Move, Board)) -> M.Map Hash (M.Map Move [GamePosInfo])
  f (n, (mv, bd)) = M.singleton (encode bd) . M.singleton mv . (:[]) $
    GamePosInfo {
      gpiFilename   = filename,
      gpiGameNum    = gameNum,
      gpiPlyCount   = n,
      gpiGameResult = pgnResult pgn
      }
  moves = map fst $ pgnMoves pgn

processGames :: String -> [(Int, PGN)] -> PosHashToInfo
processGames filename =
  M.unionsWith M.union . map (uncurry $ processGame filename)

main :: IO ()
main = do
  args <- getArgs
  let cClean = unlines . removeLineHeaders . lines
  print "ok"
  fileToPgnss <- onFiles args $ \ filename c ->
    case runParser (many1 pgnParser <* eof) () filename $ cClean c of
      Left err -> error $ show err
      Right pgns -> (filename, zip [1..] pgns)
  print "lol"
  let tree = M.unionsWith M.union $ map (uncurry processGames) fileToPgnss
  print "write"
  encodeFile "openingTree.bin" tree
  print "done"

module Main where

import Control.Monad
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Shelly
import Text.Shakespeare.Text (lt)

default (LT.Text, Shelly.FilePath)
type Range = [Int]
type ScreenSession = Text
data Mode = Command | Insert deriving Eq

shret :: a -> Sh a
shret = shelly.return

readInput = liftIO TIO.getLine

whoami :: Sh Text
whoami = shelly $ silently $ run "whoami" []

trim :: Text -> Text
trim = applyTrims myTrims
  where
    applyTrims srs str = foldl (flip trim') str srs
    trim' rm' = LT.replace rm' ""
    myTrims = ["\n","\r","\NUL"]

modecmd :: Mode -> Text -> Text
modecmd Command = flip LT.append "\r"
modecmd Insert = id

cmdSess :: Text -> ScreenSession -> Range -> Mode -> Sh()
cmdSess cmd' s r m = shelly $ do
  echo [lt|Sending [#{cmd'}] to #{s} windows #{show r}|]
  mapM_ (send m) r
  where
    send m' i = run_ "screen" [ "-S" , s , "-X" , "at" , LT.pack $ show i ++ "#"
                              , "stuff" , (modecmd m') cmd' ]

lineInteract :: Mode -> ScreenSession -> Range -> Sh ()
lineInteract mode sess range = shelly $ silently $ do
  echo_n [lt|#{sess}:#{show range} =<< |]
  cmd' <- readInput
  case cmd' of
    ":q" -> exit 0
    ":k" -> exit 0 -- also send exit to screens
    ":r" -> changeRange mode sess
    ":s" -> changeSession mode range
    ":i" -> lineInteract Insert sess range
    "help" -> echo [lt|Available internal commands are:
    :q    - quit
    :k    - killall
    :r    - pick new window range
    :s    - pick new session|]
    c -> cmdSess (LT.fromStrict c) sess range mode
  where
    changeRange m s = do
      r <- selectWindowRange
      lineInteract m s r
    changeSession m r = do
      s <- selectSession
      lineInteract m s r

userSessionDir :: Sh Shelly.FilePath
userSessionDir = shelly $ silently $ do
  usr <- whoami
  return $ fromText $ "/var/run/screen/S-" `LT.append` trim usr

nrSessions :: Sh Int
nrSessions = shelly $ silently $ do
  usd <- userSessionDir
  findFold fileCount 0 usd
  where
    fileCount int _ = shelly . return $ int+1

lsSessions :: Sh [(Int,ScreenSession)]
lsSessions = shelly $ silently $
  getSessionPaths >>= toSessionNames >>= indexNames
  where
    getSessionPaths = userSessionDir >>= findFold fileList []
    fileList ls' file = shret $ file : ls'
    toSessionNames = mapM (shret.sessionName.toTextIgnore)
    sessionName path' = LT.tail $ snd $ LT.breakOn "." path'
    indexNames names = shret $ zip [1..] names

selectSession :: Sh ScreenSession
selectSession = shelly $ silently $ do
  echo [lt|Please select recieving session:|]
  ss <- lsSessions
  ns <- nrSessions
  mapM_ printSessionList ss
  number <- readInput
  case decimal number of
    Right num -> case fst num of
      index | 0 < index && index <= ns -> resolveSession ss index
      _ -> echo_err [lt|Index out of range|] >> selectSession
    Left err -> echo_err (LT.pack err) >> selectSession
  where
    printSessionList (x,y) = echo [lt|(#{show x}) for #{y}|]
    resolveSession sessions index = shelly.return $ snd $ sessions !! (index-1)

selectWindowRange :: Sh [Int]
selectWindowRange = shelly $ silently $ do
  echo_n [lt|Enter first window: |]
  first <- readInput
  case decimal first of
    Right first' -> do
      echo_n [lt|Enter last window: |]
      last' <- readInput
      case decimal last' of
        Right last'' -> return [(fst$first')..(fst$last'')]
        _ -> selectWindowRange
    _ -> selectWindowRange

main :: IO ()
main = shelly $ silently $ do
  session <- selectSession
  range <- selectWindowRange
  forever $ lineInteract Command session range


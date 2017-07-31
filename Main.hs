{-# LANGUAGE QuasiQuotes #-}
import           Server.MDES
import           System.Console.Docopt
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)


main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  port <- args `getArgOrExit` (shortOption 'p')
  host <- args `getArgOrExit` (shortOption 'H')
  keyPath <- args `getArgOrExit` (shortOption 'k')
  config <- mkConfig host port keyPath
  case config of
    Left err     -> putStrLn err >> exitFailure
    Right conf -> runServer conf


getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

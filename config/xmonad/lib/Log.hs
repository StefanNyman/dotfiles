module Log (writeToFile) where

import           System.IO
import           System.Posix.User

writeToFile :: String -> IO ()
writeToFile l = do
    userName <- getLoginName
    h <- openFile ("/home/" ++ userName ++ "/.xmonad/xmonad.log") AppendMode
    hPutStrLn h l
    hClose h

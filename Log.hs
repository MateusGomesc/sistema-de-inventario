module Log where

import System.IO(IOMode(AppendMode), openFile, hPutStrLn, hClose)
import Data.Time(getCurrentTime, getCurrentTimeZone, formatTime, defaultTimeLocale, utcToZonedTime)

logFile :: FilePath
logFile = "Files/logs.txt"

logOperation :: String -> IO ()
logOperation descricao = do
    utcTime <- getCurrentTime
    timeZone <- getCurrentTimeZone
    let localTime = utcToZonedTime timeZone utcTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
    let msg = "[" ++ timestamp ++ "] " ++ descricao
    handle <- openFile logFile AppendMode
    hPutStrLn handle msg
    hClose handle

mostrarLog :: IO ()
mostrarLog = do
    conteudo <- readFile logFile
    putStrLn conteudo
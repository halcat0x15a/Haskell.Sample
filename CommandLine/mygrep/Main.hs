import System.Environment
import System.IO
import System.Exit
-- import Control.Monad (filterM)
import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.ICU.Regex as TIR
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            hPutStrLn stderr "Usage: mygrep pattern [file1] [file2] ..."
            exitWith (ExitFailure 1)
        else
            do
                hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering
                reg  <- TIR.regex [] (T.pack . head $ args)
                if length args == 1
                    then doesGrepLinesFromHandle reg stdin
                    else
                        mapM_ (\fn ->
                                withFile fn ReadMode $ \inh ->
                                    doesGrepLinesFromHandle reg inh) $ drop 1 args
                exitSuccess

matchesLine:: TIR.Regex -> T.Text -> IO Bool
matchesLine reg line =
    do
        TIR.setText reg line
        TIR.find reg 0


-- doesGrepLinesFromHandle:: TIR.Regex -> Handle -> IO ()
-- doesGrepLinesFromHandle reg inh =
--     do
--         contents <- TIO.hGetContents inh
--         filtered <- filterM (matchesLine reg) $ T.lines contents
--         mapM_ TIO.putStrLn filtered

doesGrepLinesFromHandle:: TIR.Regex -> Handle -> IO ()
doesGrepLinesFromHandle reg inh =
        do
        ineof <- hIsEOF inh
        unless ineof (do
                        line <- TIO.hGetLine inh
                        b <- matchesLine reg $! line
                        when b (TIO.putStrLn line)
                        doesGrepLinesFromHandle reg inh)

import System.Environment
import System.IO
import System.Exit
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TILO
import Text.Regex
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            hPutStrLn stderr "Usage: mygrep pattern [file1] [file2] ..."
            exitWith (ExitFailure 1)
        else
            do
                let reg = mkRegex . head $ args in
                    if length args == 1
                        then grepLinesFromHandle reg stdin
                        else
                            mapM_ (\fn ->
                                withFile fn ReadMode $ \inh ->
                                    grepLinesFromHandle reg inh) $ drop 1 args
                exitSuccess

grepLinesFromHandle:: Regex -> Handle -> IO ()
grepLinesFromHandle reg inh =
    do
        contents <- TILO.hGetContents inh
        mapM_ TILO.putStrLn $ filter (isJust . matchRegex reg . TL.unpack) $ TL.lines contents

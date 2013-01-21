import System.Environment
import System.IO
import System.Exit
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TILO
import Text.Regex.TDFA

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            hPutStrLn stderr "Usage: mygrep pattern [file1] [file2] ..."
            exitWith (ExitFailure 1)
        else
            do
                let reg = head args in
                    if length args == 1
                        then grepLinesFromHandle reg stdin
                        else
                            mapM_ (\fn ->
                                withFile fn ReadMode $ \inh ->
                                    grepLinesFromHandle reg inh) $ drop 1 args
                exitSuccess

grepLinesFromHandle:: String -> Handle -> IO ()
grepLinesFromHandle reg inh =
    do
        contents <- TILO.hGetContents inh
        mapM_ TILO.putStrLn $ filter (\line -> TL.unpack line =~ reg :: Bool) $ TL.lines contents

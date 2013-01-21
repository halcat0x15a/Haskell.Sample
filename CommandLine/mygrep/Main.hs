import System.Environment
import System.IO
import System.Exit
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
        contents <- hGetContents inh
        mapM_ putStrLn $ filter (\line -> line =~ reg :: Bool) $ lines contents

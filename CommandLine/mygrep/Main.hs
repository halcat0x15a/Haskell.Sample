import System.Environment
import System.IO
import System.Exit
import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.ICU.Regex as TIR
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
            then do
                hPutStrLn stderr "Usage: mygrep pattern file1 file2 ..."
                exitWith (ExitFailure 1)
            else
                do
                    reg  <- TIR.regex [] (T.pack . head $ args)
                    mapM_ (\fn -> 
                            withFile fn ReadMode $ \inh ->
                                mainloop reg inh) $ drop 1 args
                    exitSuccess

matchesLine:: TIR.Regex -> T.Text -> IO Bool
matchesLine reg line =
    do
        TIR.setText reg line
        TIR.find reg 0


mainloop :: TIR.Regex -> Handle -> IO ()
mainloop reg inh =
    do
        ineof <- hIsEOF inh
        unless ineof (do
                        line <- TIO.hGetLine inh
                        b <- matchesLine reg line
                        when b (TIO.putStrLn line)
                        mainloop reg inh)

import qualified System.Environment as SE
import qualified System.IO as SIO
import qualified Data.Text as T
import qualified Data.Text.ICU.Regex as TIR
import qualified Data.Text.IO as TIO
import Control.Monad (unless, when)

main :: IO ()
main = do
    args <- SE.getArgs
    reg  <- TIR.regex [] (T.pack . head $ args)
    SIO.withFile (args !! 1) SIO.ReadMode $ \inh ->
        mainloop reg inh

grep :: TIR.Regex -> T.Text -> IO Bool
grep reg line =
    do
        TIR.setText reg line
        TIR.find reg 0


mainloop :: TIR.Regex -> SIO.Handle -> IO ()
mainloop reg inh =
    do
        ineof <- SIO.hIsEOF inh
        unless ineof (do
                        line <- TIO.hGetLine inh
                        b <- grep reg line
                        when b (TIO.putStrLn line)
                        mainloop reg inh)
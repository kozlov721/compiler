import Compiler
import System.Process
import System.Directory
import GHC.IO.Exception (ExitCode(..))

toExe :: String -> String -> IO ()
toExe file out = do
    src <- readFile file
    let asmFile = out ++ ".s"
    let asm = compile src
    writeFile asmFile asm
    callProcess "gcc" [asmFile, "-o", out]

test :: String -> IO ()
test file = do
    let out = init . init $ file
    toExe file out
    (code, _, _) <- readProcessWithExitCode out [] ""
    putStr file
    case code of
        ExitFailure 42 -> putStrLn ": OK"
        _ -> putStrLn ": ERR"


main :: IO ()
main = do
    files <- listDirectory "test/resources"
    mapM_ (test . ("test/resources/"++))
        $ filter (\x -> [last . init $ x, last x] == ".c") files

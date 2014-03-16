import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Tuple
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process (system)
import Text.Printf

data TestCategory = Inference
                  | Elaboration
                  | Compilation
                  deriving (Eq, Show)

runTest cmd path = do
  putStrLn $ "Testing " ++ path
  exitCode <- system (printf "%s %s" cmd path)
  case exitCode of
    ExitSuccess   -> putStrLn "[[Success]]\n" >> return True
    -- TODO: distinguish between rejected file and compilation error
    ExitFailure _ -> putStrLn "[[Failure]]\n" >> return False

runTests category isGood testCmd = do
  putStrLn $ posneg ++ " tests for " ++ catName category ++ ":\n"
  fileList <- sort . filter relevant <$> getDirectoryContents dir
  let pathList = map (dir ++) fileList
  results <- mapM (runTest (testCmd ++ flag)) pathList
  let numSuccesses = length $ filter id results
      numFailures  = length $ filter not results
  putStrLn $ "Summary for " ++ dir ++ ":"
  putStrLn $ show numSuccesses ++ " successes, " ++ show numFailures ++ " failures."
  mapM_ (putStrLn . uncurry (++) . first oko) $ zip results fileList
    where relevant filename
            | ".mlt" `isSuffixOf` filename =
              category `elem` [Inference, Compilation]
            | ".mle" `isSuffixOf` filename =
                category == Elaboration
            | otherwise = False
          dir = subdir category isGood
          posneg = if isGood then "Positive" else "Negative"
          oko b | b == isGood = "[OK] "
                | otherwise   = "[KO] "
          flag = case category of
            Inference   -> " --elaboration-only" -- don't forget the space!
            Elaboration -> " --elaboration-only"
            Compilation -> " --compile-with-ocaml"

catName = map toLower . show
subdir category isGood =
  catName category ++ if isGood then "/good/" else "/bad/"

cleanDir category isGood = do
  let dir = subdir category isGood
  let exts Inference   = ["mle"]
      exts Elaboration = ["mlr"]
      exts Compilation = ["mle", "mlr", "ml", "cmo", "cmi"]
  let f ext = system $ "rm " ++ dir ++ "*." ++ ext
  mapM_ f $ exts category


usageMsg = "Usage: runhaskell run-tests.hs (inference|elaboration) (good|bad) (clean)?"
failWithUsage = do
  hPutStrLn stderr usageMsg
  exitWith $ ExitFailure 1
  
main = do
  args <- getArgs
  when (not (length args `elem` [2,3])) failWithUsage
  category <- case args !! 0 of
    "inference"   -> pure Inference
    "elaboration" -> pure Elaboration
    "compilation" -> pure Compilation
    _ -> failWithUsage
  isGood <- case args !! 1 of
    "good" -> pure True
    "bad"  -> pure False
    _ -> failWithUsage
  -- TODO: add option to run all tests / clean all directories
  if (length args == 3)
    then if args !! 2 == "clean"
           then cleanDir category isGood
           else failWithUsage
                
    else runTests category isGood "../src/joujou"

    

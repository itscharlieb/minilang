module Main where


import Control.Monad(forM_)
import System.FilePath(takeExtension)
import System.Directory(getDirectoryContents)
import Test.Hspec
import qualified Compiler


compile :: FilePath -> String -> Either String String
compile file text =
  case Compiler.parse file text of
    Right ast -> case Compiler.typeCheck ast of
      Right typedAst -> Right $ Compiler.generate typedAst
      Left typeError -> Left $ show typeError
    Left parseError -> Left $ show parseError


validSourcesDir :: FilePath
validSourcesDir = "programs/valid"


invalidSourcesDir :: FilePath
invalidSourcesDir = "programs/invalid"


loadPrograms :: FilePath -> IO [(String, String)]
loadPrograms directory = do
  files <- getDirectoryContents directory
  let files' = filter (\file -> ".min" == takeExtension file) files in
    let minFiles = map (\file -> directory ++ "/" ++ file) files' in
      mapM (\file -> readFile file >>= \text -> return (file, text)) minFiles


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = do
  validSources <- loadPrograms "programs/valid"
  invalidSources <- loadPrograms "programs/invalid"

  hspec $ describe "Compiler" $ do
    forM_ validSources $ \(file, text) ->
      it ("correctly lexes, parses, types, and generates program : " ++ file) $
        compile file text `shouldSatisfy` passes

    forM_ invalidSources $ \(file, text) ->
      it ("fails to generate : " ++ file) $
        compile file text `shouldSatisfy` not . passes

  where
    passes (Left _) = False
    passes (Right _) = True

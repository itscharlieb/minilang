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


invalidSyntacticSourcesDir :: FilePath
invalidSyntacticSourcesDir = "programs/invalid"


invalidSemanticSourcesDir :: FilePath
invalidSemanticSourcesDir = "programs/invalid_semantically"


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
  valid <- loadPrograms validSourcesDir
  invalidSyntactic <- loadPrograms invalidSyntacticSourcesDir
  invalidSemantic <- loadPrograms invalidSemanticSourcesDir

  hspec $ describe "Compiler" $ do
    forM_ valid $ \(file, text) ->
      it ("correctly compiles : " ++ file) $
        compile file text `shouldSatisfy` passes

    forM_ invalidSyntactic $ \(file, text) ->
      it ("fails to parse : " ++ file) $
        Compiler.parse file text `shouldSatisfy` not . passes

    forM_ invalidSemantic $ \(file, text) ->
      it ("fails to type check : " ++ file) $
        compile file text  `shouldSatisfy` not . passes

  where
    passes (Left _) = False
    passes (Right _) = True

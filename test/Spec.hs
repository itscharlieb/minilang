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


validSyntacticSourcesDir :: FilePath
validSyntacticSourcesDir = "programs/valid"


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
  validSyntactic <- loadPrograms validSyntacticSourcesDir
  invalidSyntactic <- loadPrograms invalidSyntacticSourcesDir
  invalidSemantic <- loadPrograms invalidSemanticSourcesDir

  hspec $ describe "Compiler" $ do
    forM_ validSyntactic $ \(file, text) ->
      it ("correctly parses : " ++ file) $
        Compiler.parse file text `shouldSatisfy` passes

    forM_ invalidSyntactic $ \(file, text) ->
      it ("fails to parse : " ++ file) $
        compile file text `shouldSatisfy` not . passes

    forM_ invalidSemantic $ \(file, text) ->
      it ("fails to type check : " ++ file) $
        compile file text  `shouldSatisfy` not . passes

  where
    passes (Left _) = False
    passes (Right _) = True

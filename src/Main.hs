module Main where

import System.Directory
import System.FilePath.Windows
import Data.List
import Control.Monad
import Data.Foldable

rootPath :: FilePath
rootPath = "c:/work/agent-desktop2/Projects/AgentDesktop.Website/client/elm"

testPath =
    rootPath ++ "/Main.elm"

lineIsNotTypeSig line =
    case words line of
        (name : ":" : _) -> False
        _ -> True

partitionFileObjects fileObjects =
    foldr (\(f, ext) (dirs, elm) ->
                case ext of
                    ".elm" -> (dirs, (f++".elm"):elm)
                    "" -> (f:dirs, elm)
                    _ -> (dirs, elm))
        ([], []) (splitExtension <$> fileObjects)

elmFiles :: FilePath -> IO [FilePath]
elmFiles path = do
    fileObjects <- listDirectory path
    let (dirs, elm) = prependPaths $ partitionFileObjects fileObjects
    children <- traverse elmFiles dirs
    pure $ concat (elm : children)
    where prependPaths (f, d) = (prependPath <$> f, prependPath <$> d)
          prependPath f = path ++ "/" ++ f

removeSigsFromFile :: FilePath -> IO ()
removeSigsFromFile path = do
    file <- readFile path
    let l = lines file
        f = filter lineIsNotTypeSig l
    writeFile' (inputToOutput path) $ unlines f

inputToOutput :: FilePath -> FilePath
inputToOutput filePath =
    "./output" ++ (drop (length rootPath) filePath)

writeFile' :: FilePath -> String -> IO ()
writeFile' path content =
  createDirectoryIfMissing True (takeDirectory path) >> writeFile path content

main :: IO ()
main = do
    elm <- elmFiles rootPath
    traverse_ removeSigsFromFile elm


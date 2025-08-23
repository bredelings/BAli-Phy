module System.Directory (createDirectory, getCurrentDirectory) where

import System.IO

foreign import bpcall "File:" createDirectoryRaw :: CPPString -> IO ()

createDirectory dirName = createDirectoryRaw (list_to_string dirName)

-- createDirectoryIfMissing :: FilePath -> IO ()
-- removeDirectory :: FilePath -> ()
-- removeDirectoryRecursive :: FilePath -> IO ()
-- removeDirectoryForcibly :: FilePath -> IO ()
-- removePathForcibly :: FilePath -> IO ()
-- renameDirectory :: FilePath -> FilePath -> IO ()
-- listDirectory :: FilePath -> IO [FilePath]
-- getDirectoryContents :: FilePath -> IO [FilePath]

foreign import bpcall "File:" getCurrentDirectoryRaw :: IO CPPString
getCurrentDirectory = unpack_cpp_string <$> getCurrentDirectoryRaw

foreign import bpcall "File:" setCurrentDirectory :: FilePath -> IO ()

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  cwd <- getCurrentDirectory
  setCurrentDirectory dir
  result <- action
  setCurrentDirectory cwd
  return result

foreign import bpcall "File:" removeFileRaw :: FilePath -> FilePath -> IO ()
removeFile :: FilePath -> FilePath -> IO ()
removeFile fromPath toPath = removeFileRaw (list_to_string fromPath) (list_to_string toPath)              

foreign import bpcall "File:" renameFileRaw :: FilePath -> FilePath -> IO ()
renameFile :: FilePath -> FilePath -> IO ()
renameFile fromPath toPath = renameFileRaw (list_to_string fromPath) (list_to_string toPath)              

foreign import bpcall "Path:" renamePathRaw :: FilePath -> FilePath -> IO ()
renamePath :: FilePath -> FilePath -> IO ()
renamePath fromPath toPath = renamePathRaw (list_to_string fromPath) (list_to_string toPath)              

foreign import bpcall "File:" copyFileRaw :: CPPString -> CPPString -> IO ()
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromPath toPath = copyFileRaw (list_to_string fromPath) (list_to_string toPath)

-- copyFileWithMetadata :: FilePath -> FilePath -> IO ()
-- getFileSize :: FilePath -> IO Integer
-- canonicalizePath :: FilePath -> IO FilePath
-- makeAbsolute :: FilePath -> IO FilePath
-- makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
-- doesPathExist :: FilePath -> IO Bool
-- doesFileExist :: FilePath -> IO Bool
-- doesDirectoryExist :: FilePath -> IO Bool
-- findExecutable :: String -> IO (Maybe FilePath)
-- findExecutables :: String -> IO [FilePath]
-- findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
-- findFile :: [FilePath] -> String -> IO (Maybe FilePath)
-- findFiles :: [FilePath] -> String -> IO [FilePath]
-- findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
-- findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
-- exeExtension :: String

-- createFileLink :: FilePath -> FilePath -> IO ()
-- createDirectoryLink :: FilePath -> FilePath -> IO ()
-- removeDirectoryLink :: FilePath -> IO ()
-- pathIsSymbolicLink :: FilePath -> IO Bool
-- getSymbolicLinkTarget :: FilePath -> IO FilePath
-- data Permissions
-- emptyPermissions :: Permissions
                    

-- readable :: Permissions -> Bool
-- writable :: Permissions -> Bool
-- executable :: Permissions -> Bool
-- searchable :: Permissions -> Bool
-- setOwnerReadable :: Bool -> Permissions -> Permissions
-- setOwnerWritable :: Bool -> Permissions -> Permissions
-- setOwnerExecutable :: Bool -> Permissions -> Permissions
-- setOwnerSearchable :: Bool -> Permissions -> Permissions
-- getPermissions :: FilePath -> IO Permissions
-- setPermissions :: FilePath -> Permissions -> IO ()
-- copyPermissions :: FilePath -> FilePath -> IO ()

                   
-- getAccessTime :: FilePath -> IO UTCTime
-- getModificationTime :: FilePath -> IO UTCTime
-- setAccessTime :: FilePath -> UTCTime -> IO ()

-- setModificationTime :: FilePath -> UTCTime -> IO () 

module BAliPhy.Util where

import System.FilePath
import System.Directory    

foreign import bpcall "File:" createUniqueDirectoryRaw :: CPPString -> IO CPPString

createUniqueDirectory dirName = unpack_cpp_string <$> createUniqueDirectoryRaw (list_to_string dirName)

storeFiles dirName files = sequence [renameFile file (dirName </> takeFileName file) | file <- files]

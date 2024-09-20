module System.Directory (createDirectory) where

import System.IO

foreign import bpcall "File:" createDirectoryRaw :: CPPString -> IO ()

createDirectory dirName = createDirectoryRaw (list_to_string dirName)

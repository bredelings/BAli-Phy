module BAliPhy.Util where

import System.FilePath

foreign import bpcall "File:" createUniqueDirectoryRaw :: CPPString -> IO FilePath

createUniqueDirectory dirName = createUniqueDirectoryRaw (list_to_string dirName)


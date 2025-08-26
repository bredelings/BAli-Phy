module BAliPhy.Util where

import System.FilePath

foreign import bpcall "File:" createUniqueDirectoryRaw :: CPPString -> IO CPPString

createUniqueDirectory dirName = unpack_cpp_string <$> createUniqueDirectoryRaw (list_to_string dirName)


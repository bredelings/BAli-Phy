{-# LANGUAGE NoImplicitPrelude #-}
module System.FilePath where

import Compiler.Base (String)
import Compiler.FFI.ToFromC    
import Foreign.String

type FilePath = String
{-
pathSeparator :: Char
pathSeparators :: [Char] 
isPathSeparator :: Char -> Bool
searchPathSeparator :: Char
isSearchPathSeparator :: Char -> Bool
extSeparator :: Char
isExtSeparator :: Char -> Bool

splitFileName :: FilePath -> (String, String)
-}

foreign import bpcall "File:" takeFileNameRaw :: CPPString -> CPPString

takeFileName :: FilePath -> FilePath
takeFileName = fromC takeFileNameRaw

{-
replaceFileName :: FilePath -> String -> FilePath
dropFileName :: FilePath -> FilePath
replaceBaseName :: FilePath -> String -> FilePath

takeDirectory :: FilePath -> FilePath
replaceDirectory :: FilePath -> String -> FilePath
-}

foreign import bpcall "File:combine" builtin_combine :: CPPString -> CPPString -> CPPString
combine path1 path2 = unpack_cpp_string (builtin_combine (pack_cpp_string path1) (pack_cpp_string path2))

infixr 5 </>
(</>) = combine
{-
splitPath :: FilePath -> [FilePath]
joinPath :: [FilePath] -> FilePath
splitDirectories :: FilePath -> [FilePath] 

splitDrive :: FilePath -> (FilePath, FilePath)
joinDrive :: FilePath -> FilePath -> FilePath
takeDrive :: FilePath -> FilePath
hasDrive :: FilePath -> Bool
dropDrive :: FilePath -> FilePath
isDrive :: FilePath -> Bool

hasTrailingPathSeparator :: FilePath -> Bool
addTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator :: FilePath -> FilePath

normalise :: FilePath -> FilePath
equalFilePath :: FilePath -> FilePath -> Bool
makeRelative :: FilePath -> FilePath -> FilePath

isRelative :: FilePath -> Bool
isAbsolute :: FilePath -> Bool
isValid :: FilePath -> Bool

makeValid :: FilePath -> FilePath
-}

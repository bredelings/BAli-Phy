{-# LANGUAGE NoImplicitPrelude #-}
module System.FilePath where

import Compiler.Base (String)
import Compiler.FFI.ToFromC    
import Foreign.String
import Data.Eq
import Data.Function
import Data.List
import Data.List.Extra (breakEnd)
import Data.Tuple
import System.Environment
import Compiler.IO
import Data.Functor
import Data.Maybe
import Data.Char hiding (isLetter)

type FilePath = String

infixr 7 <.>, -<.> 
infixr 5 </>

foreign import bpcall "Prelude:" isWindows :: Bool

isPosix = not isWindows
                                              
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/' 

pathSeparators :: [Char]
pathSeparators = if isWindows then "\\/" else "/"

isPathSeparator :: Char -> Bool
isPathSeparator c = (c `elem` pathSeparators)

searchPathSeparator :: Char
searchPathSeparator = if isWindows then ':' else ';'
                       
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator c = (c == searchPathSeparator)                         

extSeparator :: Char
extSeparator = '.'                

isExtSeparator c =  (c == extSeparator)

splitSearchPath :: String -> [FilePath]
splitSearchPath = f
    where
    f xs = case break isSearchPathSeparator xs of
           (pre, []    ) -> g pre
           (pre, _:post) -> g pre ++ f post

    g "" = ["." | isPosix]
    g ('\"':x@(_:_)) | isWindows && last x == '\"' = [init x]
    g x = [x]

getSearchPath :: IO [FilePath]
getSearchPath = fmap splitSearchPath (getEnv "PATH")                 


splitExtension :: FilePath -> (String, String)
splitExtension x = case nameDot of
                       "" -> (x,"")
                       _ -> (dir ++ init nameDot, extSeparator : ext)
    where
        (dir,file) = splitFileName_ x
        (nameDot,ext) = breakEnd isExtSeparator file

takeExtension = snd . splitExtension

replaceExtension x y = addExtension (dropExtension x) y

(-<.>) = replaceExtension

dropExtension = fst . splitExtension

addExtension x y = x ++ [extSeparator] ++ y

hasExtension x = not (null (takeExtension x))
                   
(<.>) = addExtension
    
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


-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x = isLower x || isUpper x

readDriveLetter :: String -> Maybe (FilePath, FilePath)
readDriveLetter (x:':':y:xs) | isLetter x && isPathSeparator y = Just $ addSlash [x,':'] (y:xs)
readDriveLetter (x:':':xs) | isLetter x = Just ([x,':'], xs)
readDriveLetter _ = Nothing


splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = span (== '/') x
splitDrive x | Just y <- readDriveLetter x = y
splitDrive x = ("",x)

addSlash :: FilePath -> FilePath -> (FilePath, FilePath)
addSlash a xs = (a++c,d)
    where (c,d) = span isPathSeparator xs


splitFileName :: FilePath -> (String, String)
splitFileName x = (if null dir then "./" else dir, name)
    where
        (dir, name) = splitFileName_ x

-- version of splitFileName where, if the FilePath has no directory
-- component, the returned directory is "" rather than "./".  This
splitFileName_ :: FilePath -> (String, String)
splitFileName_ x = (drv ++ dir, file)
    where
        (drv,pth) = splitDrive x
        (dir,file) = breakEnd isPathSeparator pth

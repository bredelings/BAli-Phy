{-# LANGUAGE NoImplicitPrelude #-}
module System.FilePath where

import Compiler.Base (String)
import Foreign.String

type FilePath = String

foreign import bpcall "File:combine" builtin_combine :: CPPString -> CPPString -> CPPString

combine path1 path2 = unpack_cpp_string (builtin_combine (pack_cpp_string path1) (pack_cpp_string path2))

infixr 5 </>
(</>) = combine

module Data.Text where

-- Note that this module is really just a hacky implementation of a boxed c++ std::string

import Foreign.Vector

data CppString
data Text = Text CppString

builtin builtin_pack 1 "pack" "Text"

pack = builtin_pack . list_to_vector

unpack (Text s) = listFromString s


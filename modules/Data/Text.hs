module Data.Text where

-- Note that this module is really just a hacky implementation of a boxed c++ std::string

import Foreign.String

data Text = Text CppString

builtin builtin_pack 1 "pack" "Text"

pack = Text . builtin_pack . list_to_vector

unpack (Text s) = unpack_cpp_string s


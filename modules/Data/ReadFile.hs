module Data.ReadFile where
{
builtin builtin_read_file 1 "read_file" "Data";
builtin string_to_double 1 "string_to_double" "Data";

read_file filename = list_from_vector $ builtin_read_file (listToString filename);

read_file_as_double filename = map string_to_double $ read_file filename;
}

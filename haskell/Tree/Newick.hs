module Tree.Newick where

-- For the format, see comments at http://evolution.genetics.washington.edu/phylip/newick_doc.html
--                                 http://evolution.genetics.washington.edu/phylip/newicktree.html
--
-- See the package BioBase.Newick at https://hackage.haskell.org/package/BiobaseNewick
--
-- See Data.Tree

import Tree

import Parse

import Data.Char

write_newick t@(Tree _ _ _ _) = write_newick_node 0 t
write_newick rt@(RootedTree t r _) = write_newick_node r t
write_newick rt@(LabelledTree t labels) = write_newick t


--FIXME: write the labels for a Labelled tree!
write_newick_node root tree = (write_branches_and_node tree (edgesOutOfNode tree root) root) ++ ";" where
    write_branches_and_node tree branches node = write_branches tree branches ++ show node

    write_branches tree [] = ""
    write_branches tree branches = "(" ++ text ++ ")" where
        text = intercalate "," $ map (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch)

data Newick = Newick (Maybe String) (Maybe Double) [Newick]

-- '' escapes to ' inside a quoted string
quoted_char = (string "''" >> return '\'')
              <|>
              satisfy (\c -> (isPrint c) && (c /= '\'') )

-- unquoted strings can't contain punctuation, and _ changes to space
unquoted_char = satisfy (\c -> isPrint c && not (c `elem` " ()[]':;,"))
                <|>
                (char '_' >> return ' ')

-- lex: quoted label
quoted_label = do string "'"
                  label <- many quoted_char
                  string "'"
                  return label

-- lex: unquoted label
unquoted_label = some unquoted_char

-- lex: label
node_label = quoted_label <|> unquoted_label


-- I don't want to REQUIRE a branch length
branch_length = ( string ":" >> spaces >> optionMaybe (token parse_double) ) <|> return Nothing

subtree = do children <- option [] descendant_list
             spaces
             node_label <- optionMaybe node_label
             spaces
             branch_length <- branch_length
             return (Newick node_label branch_length children)

descendant_list = do
  spaces
  string "("
  children <- sepBy1 subtree (string ",")
  string ")"
  return children

tree_parser = do spaces
                 t <- subtree
                 string ";"
                 spaces
                 return t

print_newick tree = print_newick_sub tree ++ ";"

print_newick_sub (Newick node branch children) = children_string ++  node_string ++ branch_string
    where
      children_string | null children = ""
                      | otherwise     = "("++ intercalate "," (map print_newick_sub children) ++ ")"
      node_string = case node of Just name -> "'"++name++"'"
                                 Nothing   -> ""
      branch_string = case branch of Just length -> ":" ++ show length
                                     Nothing -> ""

parse_newick text = runParser (do { spaces ; tree <- tree_parser ; spaces ; return tree }) text

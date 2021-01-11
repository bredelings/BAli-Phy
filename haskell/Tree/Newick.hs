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

write_newick tree@(Tree _ _ _ _) = write_newick (add_root tree 0)
write_newick rt@(RootedTree tree root _) = write_newick_node rt (\node -> show node)
write_newick (LabelledTree t@(Tree _ _ _ _) labels) = write_newick (LabelledTree (add_root t 0) labels)
write_newick (LabelledTree rt@(RootedTree tree root _) labels) = write_newick_node rt label_for_node
    where labels_array = listArray' labels
          (s,e) = bounds labels_array
          label_for_node node = if node >= s && node <= e then labels_array!node else ""

write_newick' (tree@(Tree _ _ _ _), lengths) = write_newick' (add_root tree 0, lengths)
write_newick' (rt@(RootedTree tree root _), lengths) = write_newick_node rt label_for_node where
    label_for_node node = show node ++ case parentBranch rt node of Just b -> ":" ++ show (lengths!b') where b' = min b (reverseEdge tree b)
                                                                    Nothing -> []

write_newick' (LabelledTree t@(Tree _ _ _ _) labels,lengths) = write_newick' (LabelledTree (add_root t 0) labels, lengths)
write_newick' (LabelledTree rt@(RootedTree tree root _) labels, lengths) = write_newick_node rt label_for_node
    where labels_array = listArray' labels
          (s,e) = bounds labels_array
          label_for_node_only node = if node >= s && node <= e then labels_array!node else ""
          label_for_node node = label_for_node_only node ++ case parentBranch rt node of Just b -> ":" ++ show (lengths!b') where b' = min b (reverseEdge tree b)
                                                                                         Nothing -> []

write_newick_node (RootedTree tree root _) label_for_node = (write_branches_and_node tree (edgesOutOfNode tree root) root) ++ ";" where

    write_branches_and_node tree branches node = write_branches tree branches ++ label_for_node node

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

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

-- We need to handle adding (i) root (ii) labels (iii) branch lengths.
-- Can we do this more generically?
class RootedTree t => WriteNewickNode t where
    node_info :: t -> Int -> (Maybe Int) -> (Maybe String, Maybe String)
    label_for_node :: t -> Int -> (Maybe Int) -> String

    label_for_node tree node mbranch = case node_info tree node mbranch of
                                         (Just name, Just branch) -> name ++ ":" ++ branch
                                         (Just name, Nothing)     -> name
                                         (Nothing  , Just branch) -> ":" ++ branch
                                         (Nothing  , Nothing)     -> ""

instance Tree t => WriteNewickNode (RootedTreeImp t) where
    node_info tree node _ = (Just $ show node, Nothing)

instance WriteNewickNode t => WriteNewickNode (LabelledTreeImp t) where
    node_info (LabelledTree tree labels) node branch = (label, subbranch)
        where labels_array = listArray' labels
              label | inRange (bounds labels_array) node  = Just (labels_array!node)
                    | otherwise                           = Nothing
              (sublabel, subbranch)                       = node_info tree node branch

instance WriteNewickNode t => WriteNewickNode (BranchLengthTreeImp t) where
    node_info blt@(BranchLengthTree tree lengths) node branch = (sublabel_for_node, branch_label branch)
        where sublabel_for_node = case node_info tree node branch of (sublabel,_) -> sublabel
              branch_label (Just b) = Just $ show (branch_length blt b)
              branch_label Nothing  = Nothing

instance WriteNewickNode t => WriteNewickNode (TimeTreeImp t) where
    node_info nht@(TimeTree tree _) node branch = (sublabel_for_node, branch_label branch)
        where sublabel_for_node = case node_info tree node branch of (sublabel, _) -> sublabel
              branch_label (Just b) = Just $ show (branch_length nht b)
              branch_label Nothing  = Nothing

instance (TimeTree t, WriteNewickNode t) => WriteNewickNode (RateTimeTreeImp t) where
    node_info nht@(RateTimeTree tree _) node branch = (sublabel_for_node, branch_label branch)
        where sublabel_for_node = case node_info tree node branch of (sublabel, _) -> sublabel
              branch_label (Just b) = Just $ show (branch_length nht b)
              branch_label Nothing  = Nothing

write_newick tree = write_newick_node tree (root tree)

write_newick_node tree node = write_branches_and_node tree (edgesOutOfNode tree node) node Nothing ++ ";" where

    write_branches_and_node tree branches node branch = write_branches tree branches ++ label_for_node tree node branch

    write_branches tree [] = ""
    write_branches tree branches = "(" ++ text ++ ")" where
        text = intercalate "," $ map (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch) (Just branch)

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
branch_length_p = ( string ":" >> spaces >> optionMaybe (token parse_double) ) <|> return Nothing

subtree = do children <- option [] descendant_list
             spaces
             node_label <- optionMaybe node_label
             spaces
             branch_length <- branch_length_p
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

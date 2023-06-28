module Tree.Newick where

-- For the format, see comments at http://evolution.genetics.washington.edu/phylip/newick_doc.html
--                                 http://evolution.genetics.washington.edu/phylip/newicktree.html
--
-- See the package BioBase.Newick at https://hackage.haskell.org/package/BiobaseNewick
--
-- See Data.Tree

import Tree

import Parse

import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Char
import Data.Foldable
import Data.Maybe (catMaybes)

-- We need to handle adding (i) root (ii) labels (iii) branch lengths.
-- Can we do this more generically?
class Tree t => WriteNewickNode t where
    node_info :: t -> Int -> Maybe Text
    branch_info :: t -> (Maybe Int) -> Maybe Text

    node_info _ _ = Nothing
    branch_info _ _ = Nothing

get_node_label   t node = case node_info t node of Just lab -> lab ; Nothing -> T.empty
get_branch_label t branch = case branch_info t branch of Just lab -> ':' `T.cons` lab; Nothing -> T.empty

instance WriteNewickNode TreeImp where
    node_info tree node = Just $ T.pack $ show node

instance WriteNewickNode t => WriteNewickNode (RootedTreeImp t) where
    node_info (RootedTree tree _ _) = node_info tree
    branch_info (RootedTree tree _ _) = branch_info tree

instance WriteNewickNode t => WriteNewickNode (LabelledTreeImp t) where
    node_info   tree node                         = get_label tree node
    branch_info (LabelledTree tree labels) branch = branch_info tree branch

instance WriteNewickNode t => WriteNewickNode (BranchLengthTreeImp t) where
    node_info (BranchLengthTree tree lengths) node = node_info tree node

    branch_info blt (Just b) = Just $ T.doubleToText (branch_length blt b)
    branch_info _   Nothing  = Nothing

instance (RootedTree t, WriteNewickNode t) => WriteNewickNode (TimeTreeImp t) where
    node_info (TimeTree tree _) node = node_info tree node

    branch_info nht (Just b) = Just $ T.doubleToText (branch_length nht b)
    branch_info _   Nothing  = Nothing

instance (TimeTree t, WriteNewickNode t) => WriteNewickNode (RateTimeTreeImp t) where
    node_info (RateTimeTree tree _) node = node_info tree node

    branch_info rtt (Just b) = Just $ T.doubleToText (branch_length rtt b)
    branch_info _    Nothing  = Nothing

write_newick tree = write_newick_node tree (root tree)

intercalate2 t ts = foldl1 (\x y -> x `T.append` t `T.append` y) ts

write_newick_node tree node = write_branches_and_node tree (edgesOutOfNode tree node) node Nothing `T.snoc` ';' where

    write_branches_and_node tree branches node branch = write_branches tree branches `T.append` get_node_label tree node `T.append` get_branch_label tree branch

    write_branches tree branches | null branches = T.empty
    write_branches tree branches | otherwise     = (T.singleton '(') `T.append` text `T.append` (T.singleton ')') where
        text = intercalate2 (T.singleton ',') $ fmap (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch) (Just branch)

data Newick = Newick [Newick] (Maybe Text) [String] (Maybe Double) [String]

comment = do
  char '['
  result <- many $ satisfy (\c -> c /= ']')
  char ']'
  if (head result == '&') then
      return (Just (tail result))
  else
      return Nothing

newickSpaces = fmap catMaybes $ many $ (comment <|> (oneOf " \t\n\r" >> return Nothing))

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
node_label = fmap T.pack $ quoted_label <|> unquoted_label


-- I don't want to REQUIRE a branch length
branch_length_p = do
  string ":"
  comments1 <- newickSpaces
  result <- optionMaybe (token parse_double)
  comments2 <- newickSpaces
  return (result, comments1 ++  comments2)

subtree = do children <- option [] descendant_list
             nodeComments1 <- newickSpaces
             node_label <- optionMaybe node_label
             nodeComments2 <- newickSpaces
             (branchLength, branchComments) <- (branch_length_p <|> return (Nothing,[]))
             return (Newick children node_label (nodeComments1 ++ nodeComments2) branchLength branchComments)

descendant_list = do
  newickSpaces
  string "("
  children <- sepBy1 subtree (string ",")
  string ")"
  return children

tree_parser = do newickSpaces
                 t <- subtree
                 string ";"
                 newickSpaces
                 return t

print_newick tree = print_newick_sub tree ++ ";"

print_newick_sub (Newick children node nodeComments branch branchComments) = children_string ++  node_string ++ nodeCommentsOut ++ branch_string ++ branchCommentsOut
    where
      children_string | null children = ""
                      | otherwise     = "("++ intercalate "," (map print_newick_sub children) ++ ")"
      node_string = case node of Just name -> "'"++(T.unpack name)++"'"
                                 Nothing   -> ""
      branch_string = case branch of Just length -> ":" ++ show length
                                     Nothing -> ""
      nodeCommentsOut = if null nodeComments
                        then ""
                        else concat ["[&" ++ comment ++ "]" | comment <- nodeComments]
      branchCommentsOut = if null branchComments
                        then ""
                        else concat ["[&" ++ comment ++ "]" | comment <- branchComments]

parse_newick text = runParser (do { newickSpaces ; tree <- tree_parser ; newickSpaces ; return tree }) text

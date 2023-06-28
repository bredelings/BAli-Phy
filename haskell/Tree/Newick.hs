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
import Data.List (stripPrefix)

import Data.Unique.Id
import qualified Data.IntMap as IntMap
import Data.Array

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

foreign import bpcall "Text:" quoteLabelRaw :: CPPString -> CPPString
quoteLabel (Text s) = Text $ quoteLabelRaw s

instance WriteNewickNode t => WriteNewickNode (LabelledTreeImp t) where
    node_info   tree node                         = fmap quoteLabel (get_label tree node)
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

split c "" = []
split c s  = case break (== c) s of
               (l, s') -> [l] ++ case s' of []    -> []
                                            _:s'' -> lines s''

data Comments = Comments [(Text,Maybe Text)]

(Comments cs1) +:+ (Comments cs2) = Comments (cs1 ++ cs2)

instance Show Comments where
    show (Comments []) = ""
    show (Comments cs) = "[&" ++ intercalate "," (fmap go cs)  ++ "]" where
                       go (k, Nothing) = T.unpack k
                       go (k, Just v)  = T.unpack k ++ "=" ++ T.unpack v

makeComments comments = Comments $ concatMap go comments where
    go comment = if take 5 comment == "&NHX:"
                 then fmap go' (split ':' (drop 5 comment))
                 else fmap go' (split ',' comment)
    go' comment = case break (== '=') comment of
                    (key,[]) -> (T.pack key,Nothing)
                    (key,_:value) -> (T.pack key, Just $ T.pack value)

data NewickNode = NewickNode [NewickNode] (Maybe Text) Comments (Maybe Double) Comments

data NewickTree = NewickTree Comments NewickNode

comment = do
  char '['
  result <- many $ satisfy (/= ']')
  char ']'
  if (head result == '&') then
      return (Just (tail result))
  else
      return Nothing

newickSpaces = fmap makeComments $ fmap catMaybes $ many $ (comment <|> (oneOf " \t\n\r" >> return Nothing))

-- '' escapes to ' inside a quoted string
quoted_char = (string "''" >> return '\'')
              <|>
              satisfy (\c -> (isPrint c) && (c /= '\'') )

-- unquoted strings can't contain punctuation, and _ changes to space
unquoted_char = (char '_' >> return ' ')
                <|>
                satisfy (\c -> isPrint c && not (c `elem`  " ()[]':;,"))

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
  return (result, comments1 +:+ comments2)

subtree = do children <- option [] descendant_list
             nodeComments1 <- newickSpaces
             node_label <- optionMaybe node_label
             nodeComments2 <- newickSpaces
             (branchLength, branchComments) <- (branch_length_p <|> return (Nothing, Comments []))
             return (NewickNode children
                                node_label
                                (nodeComments1 +:+ nodeComments2)
                                branchLength
                                branchComments)

descendant_list = do
  newickSpaces
  string "("
  children <- sepBy1 subtree (string ",")
  string ")"
  return children

treeParser = do comments <- newickSpaces
                node <- subtree
                string ";"
                spaces
                return $ NewickTree comments node

print_newick (NewickTree comments node) = show comments ++ " " ++ print_newick_sub node ++ ";"

replace from to []     = []
replace from to string = case stripPrefix from string of
                           Just rest -> to ++ replace from to rest
                           Nothing   -> head string : replace from to (tail string)

quoteName :: String -> String
quoteName name = if any (\l -> elem l  "()[]':;,") name then
                     "'"++(replace "'" "''" name)++"'"
                 else
                     replace " " "_" name


print_newick_sub (NewickNode children node nodeComments branch branchComments) =
    children_string ++  node_string ++ (show nodeComments) ++ branch_string ++ (show branchComments)
    where
      children_string | null children = ""
                      | otherwise     = "("++ intercalate "," (map print_newick_sub children) ++ ")"
      node_string = case node of Just name -> quoteName (T.unpack name)
                                 Nothing   -> ""
      branch_string = case branch of Just length -> ":" ++ show length
                                     Nothing -> ""

parse_newick text = runParser treeParser text


-- edge comments come from the child
data Info = Info { i_nodes :: [Node],
                   i_edges :: [Edge],
                   i_labels :: [(Int,Maybe Text)],
                   i_lengths :: [(Int,Maybe Double)],
                   i_nodeComments :: [(Int,Comments)],
                   i_edgeComments :: [(Int,Comments)]
                 }

combineInfo (Info ns1 es1 ls1 bls1 ncs1 ecs1) (Info ns2 es2 ls2 bls2 ncs2 ecs2) =
    Info (ns1++ns2) (es1++es2) (ls1++ls2) (bls1++bls2) (ncs1++ncs2) (ecs1++ecs2)

getEdge ids node@(NewickNode _ _ _ branchLength branchComments) nodeId index = (edgeId, edgeInfo `combineInfo` childInfo) where
    edgeId = hashedId $ idFromSupply ids
    (ids',childIds) = splitIdSupply ids
    reverseEdgeId = hashedId $ idFromSupply ids'
    edgeInfo = Info [] [eToChild,eFromChild] [] [(edgeId,branchLength),(reverseEdgeId,branchLength)] [] [(edgeId,branchComments),(reverseEdgeId,branchComments)]
    eToChild = Edge nodeId index targetId reverseEdgeId edgeId
    eFromChild = Edge targetId 0 nodeId edgeId reverseEdgeId
    (targetId, childInfo) = getNode childIds node (Just reverseEdgeId)

getNode ids (NewickNode children nodeLabel nodeComments _ _) parentEdge = (nodeId,foldr combineInfo nodeInfo childInfo)
    where nodeInfo = Info [node] [] [(nodeId,nodeLabel)] [] [(nodeId,nodeComments)] []
          node = Node nodeId outEdges
          nodeId = hashedId $ idFromSupply ids
          outEdges = listArray' edgeIds
          edgeIds = case parentEdge of Just e -> (e:childEdgeIds) ; Nothing -> childEdgeIds
          firstChildIndex = case parentEdge of Nothing -> 0; _ -> 1
          childIndices = [firstChildIndex .. ]
          (childEdgeIds, childInfo) = unzip [getEdge childIds childNewick nodeId index
                                                 | (childNewick, childIds, index) <- zip3 children (splitIdSupplyL ids) childIndices]


newickToTree (NewickTree treeComments node) = do
  ids <- initIdSupply 'n'

  let (rootId, info) = getNode ids node Nothing
      nodes = IntMap.fromList [(node_name node, node) | node <- i_nodes info]
      edges = IntMap.fromList [(edge_name edge, edge) | edge <- i_edges info]
      tree = Tree nodes edges
      -- These SHOULD have all the nodes / edges... but maybe we should use (getNodesSet tree & fromSet _) to make sure.
      labels = IntMap.fromList (i_labels info) -- this is IntMap (Maybe Double)
      lengths = IntMap.fromList (i_lengths info)
      nodeComments = IntMap.fromList (i_nodeComments info)
      edgeComments = IntMap.fromList (i_edgeComments info)

  return (tree, rootId, labels, lengths, nodeComments, edgeComments, treeComments)

newickToBranchLengthTree newick = do
  (tree, rootId, labels, lengths, nodeComments, edgeComments, treeComments) <- newickToTree newick
  let lengths2 = fmap (fromMaybe 0) lengths
      bltree = (LabelledTree (BranchLengthTree (add_root rootId tree) lengths2) labels)
  return (bltree, nodeComments, edgeComments, treeComments)

readBranchLengthTree filename = do
  text <- readFile filename
  newickToBranchLengthTree (parse_newick text)

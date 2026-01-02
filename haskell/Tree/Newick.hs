module Tree.Newick where

-- For the format, see comments at http://evolution.genetics.washington.edu/phylip/newick_doc.html
--                                 http://evolution.genetics.washington.edu/phylip/newicktree.html
--
-- See the package BioBase.Newick at https://hackage.haskell.org/package/BiobaseNewick
--
-- See Data.Tree

import Tree

import Parse

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.List (stripPrefix)

import Data.Unique.Id
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Array
import Data.Text.Display

-- We need to handle adding (i) root (ii) labels (iii) branch lengths.
-- Can we do this more generically?
class IsTree t => WriteNewickNode t where
    node_info :: t -> Int -> Text
    branch_info :: t -> (Maybe Int) -> Text

    node_info _ _ = T.empty
    branch_info _ _ = T.empty

get_node_label   t node = T.append (node_info t node) (attributesText $ getNodeAttributes t node)
get_branch_label t branch@(Just edge) = let text = (branch_info t branch) `T.append` (attributesText $ getEdgeAttributes t edge)
                                        in if T.null text then text else T.singleton ':' `T.append` text
get_branch_label t Nothing = T.empty

instance Display l => WriteNewickNode (Tree l) where
    node_info   tree node                         = case getLabel tree node of
                                                      Just label -> quoteLabel (display label)
                                                      Nothing -> T.empty

instance WriteNewickNode t => WriteNewickNode (WithRoots t) where
    node_info (WithRoots tree _ _) = node_info tree
    branch_info (WithRoots tree _ _) = branch_info tree

foreign import ecall "Text:" quoteLabelRaw :: CPPString -> CPPString
quoteLabel l = T.fromCppString $ quoteLabelRaw $ T.toCppString l

instance WriteNewickNode t => WriteNewickNode (WithBranchLengths t) where
    node_info (WithBranchLengths tree lengths) node = node_info tree node

    branch_info blt (Just b) = T.doubleToText (branchLength blt b)
    branch_info _   Nothing  = T.empty

instance (HasRoot t, WriteNewickNode t) => WriteNewickNode (WithNodeTimes t) where
    node_info (WithNodeTimes tree _) node = node_info tree node

    branch_info nht (Just b) = T.doubleToText (branchLength nht b)
    branch_info _   Nothing  = T.empty

instance (HasBranchLengths t, WriteNewickNode t) => WriteNewickNode (WithBranchRates t) where
    node_info (WithBranchRates tree _) node = node_info tree node

    branch_info rtt (Just b) = T.doubleToText (branchLength rtt b)
    branch_info _    Nothing  = T.empty

writeNewick_rooted tree = let nodes = writeNewick_node tree (root tree)
                              attributes = attributesText $ getAttributes tree
                          in if T.null attributes
                             then nodes
                             else T.concat [attributes,T.singleton ' ',nodes]

writeNewick tree = writeNewick_rooted (makeRooted tree)

intercalate2 t ts = foldl1 (\x y -> x `T.append` t `T.append` y) ts

writeNewick_node tree node = write_branches_and_node tree (edgesOutOfNode tree node) node Nothing `T.snoc` ';' where

    write_branches_and_node tree branches node branch = write_branches tree branches `T.append` get_node_label tree node `T.append` get_branch_label tree branch

    write_branches tree branches | null branches = T.empty
    write_branches tree branches | otherwise     = (T.singleton '(') `T.append` text `T.append` (T.singleton ')') where
        text = intercalate2 (T.singleton ',') $ fmap (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch) (Just branch)

split c "" = []
split c s  = case break (== c) s of
               (l, s') -> [l] ++ case s' of []    -> []
                                            _:s'' -> lines s''

makeAttributes comments = Attributes $ concatMap go comments where
    go comment = if take 5 comment == "&NHX:"
                 then fmap go' (split ':' (drop 5 comment))
                 else fmap go' (split ',' comment)
    go' comment = case break (== '=') comment of
                    (key,[]) -> (T.pack key,Nothing)
                    (key,_:value) -> (T.pack key, Just $ T.pack value)

data NewickNode = NewickNode [NewickNode] (Maybe Text) Attributes (Maybe Double) Attributes

data NewickTree = NewickTree Attributes NewickNode

comment = do
  char '['
  result <- many $ satisfy (/= ']')
  char ']'
  if (head result == '&') then
      return (Just (tail result))
  else
      return Nothing

newickSpaces = fmap makeAttributes $ fmap catMaybes $ many $ (comment <|> (satisfy isSpace >> return Nothing))

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
  attributes1 <- newickSpaces
  result <- optionMaybe (token parse_double)
  attributes2 <- newickSpaces
  return (result, attributes1 +:+ attributes2)

subtree = do nodeAttributes1 <- newickSpaces
             children <- option [] descendant_list
             nodeAttributes2 <- newickSpaces
             node_label <- optionMaybe node_label
             nodeAttributes3 <- newickSpaces
             (branchLength, branchAttributes) <- (branch_length_p <|> return (Nothing, Attributes []))
             return (NewickNode children
                                node_label
                                (nodeAttributes1 +:+ nodeAttributes2 +:+ nodeAttributes3)
                                branchLength
                                branchAttributes)

descendant_list = do
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
quoteName name = if any (\l -> elem l  "_()[]':;,") name then
                     "'"++(replace "'" "''" name)++"'"
                 else
                     replace " " "_" name


print_newick_sub (NewickNode children node nodeAttributes branch branchAttributes) =
    children_string ++  node_string ++ (show nodeAttributes) ++ branch_string ++ (show branchAttributes)
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
                   i_nodeAttributes :: [(Int,Attributes)],
                   i_edgeAttributes :: [(Int,Attributes)]
                 }

combineInfo (Info ns1 es1 ls1 bls1 ncs1 ecs1) (Info ns2 es2 ls2 bls2 ncs2 ecs2) =
    Info (ns1++ns2) (es1++es2) (ls1++ls2) (bls1++bls2) (ncs1++ncs2) (ecs1++ecs2)

getEdge ids node@(NewickNode _ _ _ branchLength branchAttributes) nodeId = (edgeId, edgeInfo `combineInfo` childInfo) where
    edgeId = (hashedId $ idFromSupply ids)+1
    reverseEdgeId = reverseEdge edgeId
    (ids',childIds) = splitIdSupply ids
    edgeInfo = Info [] [eToChild,eFromChild] [] [(edgeId,branchLength),(reverseEdgeId,branchLength)] [] [(edgeId,branchAttributes),(reverseEdgeId,branchAttributes)]
    eToChild = Edge nodeId targetId edgeId
    eFromChild = Edge targetId nodeId reverseEdgeId
    (targetId, childInfo) = getNode childIds node (Just reverseEdgeId)

getNode ids (NewickNode children nodeLabel nodeAttributes _ _) parentEdge = (nodeId,foldr combineInfo nodeInfo childInfo)
    where nodeInfo = Info [node] [] [(nodeId,nodeLabel)] [] [(nodeId,nodeAttributes)] []
          node = Node nodeId outEdges
          nodeId = hashedId $ idFromSupply ids
          outEdges = IntSet.fromList edgeIds
          edgeIds = case parentEdge of Just e -> (e:childEdgeIds) ; Nothing -> childEdgeIds
          (childEdgeIds, childInfo) = unzip [getEdge childIds childNewick nodeId
                                                 | (childNewick, childIds) <- zip children (splitIdSupplyL ids)]


newickToTree (NewickTree treeAttributes node) = do
  ids <- initIdSupply 'n'

  let (rootId, info) = getNode ids node Nothing
      nodes = IntMap.fromList [(nodeName node, node) | node <- i_nodes info]
      edges = IntMap.fromList [(edgeName edge, edge) | edge <- i_edges info]
      -- These SHOULD have all the nodes / edges... but maybe we should use (getNodesSet tree & fromSet _) to make sure.
      labels = IntMap.fromList (i_labels info) -- this is IntMap (Maybe Double)
      lengths = IntMap.fromList (i_lengths info)
      nodeAttributes = IntMap.fromList (i_nodeAttributes info)
      edgeAttributes = IntMap.fromList (i_edgeAttributes info)
      tree = Tree $ Forest $ Graph nodes edges labels nodeAttributes edgeAttributes treeAttributes
      rooted_tree = addRoot rootId tree

  return (rooted_tree, lengths)

newickToBranchLengthTree newick = do
  (tree, lengths) <- newickToTree newick
  let lengths2 = fmap (fromMaybe 0) lengths
  return (WithBranchLengths tree lengths2)

readTreeTopology filename = do
  text <- readFile filename
  (topology, lengths) <- newickToTree (parse_newick text)
  return topology

readBranchLengthTree filename = do
  text <- readFile filename
  newickToBranchLengthTree (parse_newick text)

-- Perhaps we should try and strip off the WithBranchLengths from a general tree...
-- We would need a type family like Unrooted to walk all the modifiers.
readTimeTree filename = do
  text <- readFile filename
  (tree, lengths) <- newickToTree (parse_newick text)
  let lengths2 = fmap (fromMaybe 0) lengths
      nodeTimes = getNodesSet tree & IntMap.fromSet nodeTime
      nodeTime node = case branchToParent tree node of
                        Nothing -> 0
                        Just b -> let p = targetNode tree b
                                  in (nodeTimes IntMap.! p) + (lengths2 IntMap.! b)
      present = maximum nodeTimes
      nodeAges = fmap (\t -> present - t) nodeTimes

  return tree

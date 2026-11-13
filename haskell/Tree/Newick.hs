module Tree.Newick (module Tree.Newick, module Tree.Newick.Write) where

-- For the format, see comments at http://evolution.genetics.washington.edu/phylip/newick_doc.html
--                                 http://evolution.genetics.washington.edu/phylip/newicktree.html
--
-- See the package BioBase.Newick at https://hackage.haskell.org/package/BiobaseNewick
--
-- See Data.Tree

import Tree.Types
import Tree.Newick.Write

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

-- Track expected closing characters as a stack, so delimiters split fields only at the
-- outer level and mismatched or crossed grouping characters are rejected.
splitGrouped _ "" = []
splitGrouped delimiter input = go [] [] input where
    go [] field [] = [reverse field]
    go _ _ [] = error "Unclosed grouping character in Newick attribute comment"
    go expected field (character:rest)
      | character == delimiter && null expected = reverse field : go [] [] rest
      | character `elem` "({[" = go (closing character : expected) (character:field) rest
      | character `elem` ")}]" =
          case expected of
            closingCharacter:remaining | character == closingCharacter ->
                go remaining (character:field) rest
            _ -> error "Mismatched grouping character in Newick attribute comment"
      | otherwise = go expected (character:field) rest
    closing '(' = ')'
    closing '{' = '}'
    closing '[' = ']'

makeAttributes comments = Attributes $ concatMap go comments where
    go comment = if take 5 comment == "&NHX:"
                 then fmap go' (splitGrouped ':' (drop 5 comment))
                 else fmap go' (splitGrouped ',' comment)
    go' comment = case break (== '=') comment of
                    (key,[]) -> (T.pack key,Nothing)
                    (key,_:value) -> (T.pack key, Just $ T.pack value)

data NewickNode = NewickNode [NewickNode] (Maybe Text) Attributes (Maybe Double) Attributes

data NewickTree = NewickTree Attributes NewickNode

-- Parse balanced comments while preserving nested brackets in attribute values.
comment = do
  char '['
  result <- fmap concat $ many commentPart
  char ']' <?> "']' to close comment"
  case result of
    '&':attributes -> return (Just attributes)
    _              -> return Nothing
  where
    commentPart = nestedComment <|> fmap (:[]) (satisfy (/= ']'))
    -- Preserve the brackets around nested content so grouped attribute values can inspect them.
    nestedComment = do
      char '['
      contents <- fmap concat $ many commentPart
      char ']' <?> "']' to close nested comment"
      return ('[':contents ++ "]")

newickSpaces = fmap makeAttributes $ fmap catMaybes $ many $ (comment <|> (satisfy isSpace >> return Nothing))

-- A single quote ends the label, so a failed doubled-quote match must let the
-- enclosing repetition stop before the closing delimiter.
quoted_char = (try (string "''") >> return '\'')
              <|>
              satisfy (\c -> (isPrint c) && (c /= '\'') )

-- NOTE: Traditional Newick converts an unquoted '_' to a space. Preserve it
-- because common tools write sequence identifiers with literal underscores.
unquoted_char = satisfy (\c -> isPrint c && not (c `elem`  " ()[]':;,"))

-- lex: quoted label
quoted_label = do string "'"
                  label <- many quoted_char
                  char '\'' <?> "closing quote for node label"
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
  char ')' <?> "')' to close descendant list"
  return children

treeParser = do comments <- newickSpaces
                node <- subtree
                char ';' <?> "';' after the tree"
                spaces
                return $ NewickTree comments node

print_newick (NewickTree comments node) = show comments ++ " " ++ print_newick_sub node ++ ";"

replace from to []     = []
replace from to string = case stripPrefix from string of
                           Just rest -> to ++ replace from to rest
                           Nothing   -> head string : replace from to (tail string)

-- NOTE: Leave '_' unquoted because some common tools do not decode quoted
-- labels correctly; quote actual spaces because this parser preserves '_'.
quoteName :: String -> String
quoteName name = if any (\l -> elem l  "()[]':;, ") name then
                     "'"++(replace "'" "''" name)++"'"
                 else
                     name


print_newick_sub (NewickNode children node nodeAttributes branch branchAttributes) =
    children_string ++  node_string ++ (show nodeAttributes) ++ branch_string ++ (show branchAttributes)
    where
      children_string | null children = ""
                      | otherwise     = "("++ intercalate "," (map print_newick_sub children) ++ ")"
      node_string = case node of Just name -> quoteName (T.unpack name)
                                 Nothing   -> ""
      branch_string = case branch of Just length -> ":" ++ show length
                                     Nothing -> ""

parse_newick text = runParserFor "Newick tree" treeParser text

-- Include the source filename when Newick text came from a file.
parse_newick_from_file filename text = runParserFor ("Newick tree from " ++ show filename) treeParser text


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
  (topology, lengths) <- newickToTree (parse_newick_from_file filename text)
  return topology

readBranchLengthTree filename = do
  text <- readFile filename
  newickToBranchLengthTree (parse_newick_from_file filename text)

-- Perhaps we should try and strip off the WithBranchLengths from a general tree...
-- We would need a type family like Unrooted to walk all the modifiers.
-- Measure node ages backwards from the deepest tip; shorter paths retain older tip ages.
readTimeTree filename = do
  text <- readFile filename
  (tree, lengths) <- newickToTree (parse_newick_from_file filename text)
  let lengths2 = fmap (fromMaybe 0) lengths
      nodeTimes = getNodesSet tree & IntMap.fromSet nodeTime
      nodeTime node = case branchToParent tree node of
                        Nothing -> 0
                        Just b -> let p = targetNode tree b
                                  in (nodeTimes IntMap.! p) + (lengths2 IntMap.! b)
      present = maximum nodeTimes
      nodeAges = fmap (\t -> present - t) nodeTimes

  return $ time_tree tree nodeAges

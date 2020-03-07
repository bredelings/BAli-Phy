module Tree.Newick where

import Tree

write_newick t@(Tree _ _ _ _) = write_newick_node 0 t
write_newick rt@(RootedTree t r _) = write_newick_node r t

write_newick_node root tree = (write_branches_and_node tree (edgesOutOfNode tree root) root) ++ ";" where
    write_branches_and_node tree branches node = write_branches tree branches ++ show node

    write_branches tree [] = ""
    write_branches tree branches = "(" ++ text ++ ")" where
        text = intercalate "," $ map (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch)



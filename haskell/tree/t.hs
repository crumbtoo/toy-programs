import Tree

main = do
    let t = Tree.fromList ([4,6,7,7,5,1,3,1,2,5,5,0,8,5,0,5,7,5,6,9,5] :: [Int])
    Tree.printTree t
    print $ Tree.toList t


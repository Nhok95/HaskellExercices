data Tree a = Node a (Tree a) (Tree a) 
             | Empty deriving (Show)

--funció que, donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
size :: Tree a -> Int
size (Node a f1 f2) = 1 + size(f1) + size(f2)
size (Empty) = 0

--funció que, donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
height :: Tree a -> Int 
height (Node a f1 f2) = 1 + max (height f1)  (height f2)
height (Empty) = 0

--funció que, donat dos arbres, indiqui si són el mateix.
equal :: Eq a => Tree a -> Tree a -> Bool 
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a af1 af2) (Node b bf1 bf2) = 
    a == b && equal af1 bf1 && equal af2 bf2


--funció que, donat un arbres, indiqui si són el isomorfs, és a dir, si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
isomorphic :: Eq a => Tree a -> Tree a -> Bool 
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a af1 af2) (Node b bf1 bf2) =
    a == b && ((isomorphic af1 bf1  && isomorphic af2 bf2) || (isomorphic af1 bf2  && isomorphic af2 bf1))



--funció que, donat un arbre, retorni el seu recorregut en pre-ordre.
preOrder :: Tree a -> [a] 
preOrder Empty = []
preOrder (Node a f1 f2) =  a:(preOrder f1) ++ (preOrder f2)

--funció que, donat un arbre, retorni el seu recorregut en post-ordre.
postOrder :: Tree a -> [a] 
postOrder Empty = []
postOrder (Node a f1 f2) = (postOrder f1) ++ (postOrder f2) ++ [a]

--funció que, donat un arbre, retorni el seu recorregut en in-ordre.
inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node a f1 f2) = (inOrder f1) ++ [a] ++ (inOrder f2) 


--funció que, donat un arbre, retorni el seu recorregut per nivells. 
{-
(malament -> nomes per arbres de profunditat 2)
bfs2 :: Tree a -> Tree a -> [a]
bfs2 Empty Empty = []
bfs2 Empty (Node b bf1 bf2) = [b]
bfs2 (Node a af1 af2) Empty = [a]
bfs2 (Node a af1 af2) (Node b bf1 bf2) = a:[b]  ++ (bfs af1 af2) ++ (bfs bf1 bf2)

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node a f1 f2) =  a: (bfs2 f1 f2)

-}
bfs :: [Tree t] -> [t]
bfs [] = []
bfs (Empty:xs) = bfs xs
bfs ((Node x l r):xs) = x : (bfs $ xs ++ [l,r])

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]


--funció que, donat el recorregut en pre-ordre d’un arbre i el recorregut en in-ordre del mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build p@(px : pxs) i = Node px (build lp li) (build rp ri)
    where (li,_:ri) = span (/=px) i
          (lp,rp) = splitAt (length li) pxs


--funció que, donats dos arbres, retorni la seva superposició utilitzant una funció. Superposar dos arbres amb una funció consisteix en posar els dos arbres l’un damunt de l’altre i combinar els nodes doble resultants amb la funció donada o deixant els nodes simples tal qual.
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a 
overlap f Empty            Empty            = Empty
overlap f (Node a f1a f2a) Empty            = Node a f1a f2a
overlap f Empty            (Node b f1b f2b) = Node b f1b f2b 
overlap f (Node a f1a f2a) (Node b f1b f2b) = Node c (overlap f f1a f1b) (overlap f f2a f2b)
    where c = f a b




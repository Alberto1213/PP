{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es) --pereche de multimi 

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes (a, b) = a -- multimea de noduri

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges (a, b) = b --multimea de muchii

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
--outNeighbors node graph = undefined
outNeighbors node (ns, es) = S.fromList (map(snd) (filter(\(x, y)->x == node) (S.toList es))) -- functia din interior primeste x si y si intoarce true daca x,y e muchie care pleaca din node . Pentru fiecare pereche
                                                                                                   -- element din lista ia noduri si le aplica fct snd ca sa ia doar al doilea element , iar acum avem lista cu toate elementele
                                                                                                    --deci le facem multime 

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
--inNeighbors node graph = undefined
inNeighbors node (ns, es) = S.fromList (map(fst) (filter(\(x, y) -> y == node) (S.toList es))) -- aceeasi chestie ca mai sus

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
--removeNode node graph = undefined
removeNode node (ns, es) = fromComponents (filter(\x -> x/=node) (S.toList ns)) (filter(\(x, y) -> x/=node && y/=node) (S.toList es)) --deci facem multimi de la listele de noduri si muchii. filtram multimile de noduri
                                                                                                                                      --si muchii a i nodurile sa fie dif de nodul pe care vrem sa l stergem si muchile sa nu aiba
                                                                                                                                      -- ca nod nodul pe care vrem sa l stergem

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news (ns, es) = fromComponents (newNodes old news (S.toList ns)) (newEdges old news (S.toList es))
    where 
        newNodes old news ns = (filter(\x -> x/=old) ns) ++news --sterg nodul vechi si adaug nodurile noi
        newEdges old news es = foldr (++) [] (map(\(x,y)-> if x==old then [(alt, y) | alt<-news] else if y==old then [(x, alt) | alt<-news] else [(x,y)]) es)  -- inlocuieste toate muchile care contine node in stanga sau 
                                                                                                                                                                --sau dreapta cu news sau lasa muchia intacta in caz contrat
        --alta abordare valida dar cu garzi
        {-newEdges old news [] = []
        newEdges old news ((x, y) : tail) 
            | x == old = [(alt, y) | alt<-news] ++ newEdges old news tail
            | y == old = [(x, alt) | alt<-news] ++ newEdges old news tail
            | otherwise = [(x, y)] ++ newEdges old news tail-}
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node (ns, es) = fromComponents (newNodes prop node (S.toList ns)) (newEdges prop node (S.toList es))
    where
        newNodes prop node ns 
            | foldr (||) (False) (map prop ns) == True = filter(\x->(prop x == False)) ns ++ [node]  -- verifica prop pe toata multimea de noduri si daca exista un nod cu prop adaugam nodul nou si l stergem pe ala vechi
            | otherwise = ns
        newEdges prop node es = map modifyEdge es
            where
                modifyEdge (x,y) 
                    | prop x && prop y = (node, node)
                    | prop x && (prop y == False) = (node, y)
                    | (prop x == False) && prop y = (x, node)
                    | otherwise = (x, y)    
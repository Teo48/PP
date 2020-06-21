{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {
    state :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    children :: [Node s a]
}

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = state node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = parent $ node

nodeDepth :: Node s a -> Int
nodeDepth node = depth $ node 

nodeAction :: Node s a -> Maybe a
nodeAction node = action $ node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children $ node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

instance (Eq s) => Eq (Node s a) where
    (Node s1 _ _ _ _) == (Node s2 _ _ _ _) = s1 == s2

createStateSpaceHelper :: (ProblemState s a, Eq s) => Maybe (Node s a) -> Int -> (Maybe a, s) -> Node s a
createStateSpaceHelper dad adancime (dir, stare) = root
    where {
        root = Node stare dir dad adancime copil;
        copil = (map (\ (actiune, child) -> createStateSpaceHelper (Just root) (adancime + 1) (Just actiune, child)) (successors stare));
    }

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace stare = createStateSpaceHelper Nothing 0 (Nothing, stare)

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsHelper :: [(Node s a)]-> [([Node s a], [Node s a])]
bfsHelper [] = []
bfsHelper x@(_ : xs) = (x, xs ++ x) : (bfsHelper xs)

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs first = bfsHelper [first]
        


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath (Node stare actiune dad _ _) = case actiune of
    Nothing -> [(Nothing, stare)]
    _ -> (extractPath $ fromJust dad) ++ [(actiune, stare)]


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined

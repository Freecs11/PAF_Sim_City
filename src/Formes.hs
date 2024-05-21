module Formes where

import GameData

-- formes functions and invariants/pre/post conditions
-- instances also ( functor, applicative, monad, foldable, traversable, etc. )

getFormeCoord :: Forme -> Coord
getFormeCoord (HSegment c _) = c
getFormeCoord (VSegment c _) = c
getFormeCoord (Rectangle c _ _) = c


getXY :: Coord -> (Int, Int)
getXY (C x y) = (x, y)


moveForme :: Coord -> Forme -> Forme
moveForme c (HSegment _ l) = HSegment c l
moveForme c (VSegment _ l) = VSegment c l
moveForme c (Rectangle _ w h) = Rectangle c w h



limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y, y + l - 1, x, x)
limites (Rectangle (C x y) l h) = (y, y + h - 1, x, x + l - 1)


-- precondition de limites :
-- que la forme soit valide et que les coordonnées soient des entiers valides 
prop_limites_precondition :: Forme -> Bool
prop_limites_precondition (HSegment _ l) = l > 0
prop_limites_precondition (VSegment _ l) = l > 0
prop_limites_precondition (Rectangle _ w h) = w > 0 && h > 0

-- postcondition de limites :
-- les coordonnées de la forme sont valides et les limites sont correctes
prop_limites_postcondition :: Forme -> Bool
prop_limites_postcondition form@(HSegment (C x y) l) = 
    limites form == (y, y, x, x + l - 1)
prop_limites_postcondition form@(VSegment (C x y) l) = 
    limites form == (y, y + l - 1, x, x)
prop_limites_postcondition form@(Rectangle (C x y) w h) = 
    limites form == (y, y + h - 1, x, x + w - 1)



appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x' y') l) = y == y' && x >= x' && x < x' + l
appartient (C x y) (VSegment (C x' y') l) = x == x' && y >= y' && y < y' + l
appartient (C x y) (Rectangle (C x' y') l h) = x >= x' && x < x' + l && y >= y' && y < y' + h

-- checking that a coordinate is either within the Forme or directly adjacent (but not part of it).
prop_appartient_invariant :: Coord -> Forme -> Bool
prop_appartient_invariant coord@(C x y) forme =
    appartient coord forme || 
    not (appartient (C (x-1) y) forme && appartient (C (x+1) y) forme && 
         appartient (C x (y-1)) forme && appartient (C x (y+1)) forme)

-- prop_appartient_precondition: Ensures the dimensions are positive.
prop_appartient_precondition :: Coord -> Forme -> Bool
prop_appartient_precondition (C x y) (HSegment (C x' y') l) = l > 0
prop_appartient_precondition (C x y) (VSegment (C x' y') l) = l > 0
prop_appartient_precondition (C x y) (Rectangle (C x' y') w h) = w > 0 && h > 0

prop_appartient_postcondition :: Coord -> Forme -> Bool
prop_appartient_postcondition coord@(C x y) forme = 
    appartient coord forme == (case forme of
        HSegment (C x' y') l -> y == y' && x >= x' && x < x' + l
        VSegment (C x' y') l -> x == x' && y >= y' && y < y' + l
        Rectangle (C x' y') w h -> x >= x' && x < x' + w && y >= y' && y < y' + h)


adjacent :: Coord -> Forme -> Bool
adjacent (C x y) (HSegment (C x' y') l) = (x == x' + l || x == x' - 1) && y >= y' && y < y' + 1
adjacent (C x y) (VSegment (C x' y') l) = (y == y' + l || y == y' - 1) && x >= x' && x < x' + 1
adjacent (C x y) (Rectangle (C x' y') l h) = 
    ((x == x' + l || x == x' - 1) && (y >= y' && y < y' + h)) || 
    ((y == y' + h || y == y' - 1) && (x >= x' && x < x' + l))


prop_adjacent_precondition :: Coord -> Forme -> Bool
prop_adjacent_precondition (C x y) (HSegment (C x' y') l) = l > 0
prop_adjacent_precondition (C x y) (VSegment (C x' y') l) = l > 0
prop_adjacent_precondition (C x y) (Rectangle (C x' y') w h) = w > 0 && h > 0


prop_adjacent_postcondition :: Coord -> Forme -> Bool
prop_adjacent_postcondition coord@(C x y) forme = 
    adjacent coord forme == (case forme of
        HSegment (C x' y') l -> (x == x' + l || x == x' - 1) && y >= y' && y < y' + 1
        VSegment (C x' y') l -> (y == y' + l || y == y' - 1) && x >= x' && x < x' + 1
        Rectangle (C x' y') w h -> (x == x' + w || x == x' - 1) && y >= y' && y < y' + h || (y == y' + h || y == y' - 1) && x >= x' && x < x' + w)


-- invariant à revoir ..... là c'est un peu n'importe quoi
prop_adjacent_invariant :: Coord -> Forme -> Bool
prop_adjacent_invariant coord@(C x y) forme =
    appartient coord forme ||  -- si le coord est dans la forme
    (adjacent coord forme && not (appartient coord forme)) ||  -- si le coord est adjacent à la forme mais pas dedans
    not (adjacent (C (x-1) y) forme && adjacent (C (x+1) y) forme &&  -- ou alors c'est pas adjacent ( test de tous les côtés )
         adjacent (C x (y-1)) forme && adjacent (C x (y+1)) forme)


collision :: Forme -> Forme -> Bool
collision f1 f2 = let (n1, s1, o1, e1) = limites f1
                      (n2, s2, o2, e2) = limites f2
                  in n1 <= s2 && s1 >= n2 && o1 <= e2 && e1 >= o2 -- bouding box collision simple



-- collision invariant and postcondition
prop_collision_invariant :: Forme -> Forme -> Bool
prop_collision_invariant f1 f2 = collision f1 f2 == collision f2 f1  -- invariant sur la symétrie de la collision , je vois pas d'autres invariants/ à revoir

--  un peu redondant mais bon ... ( c'est pour les tests )
prop_collision_postcondition :: Forme -> Forme -> Bool
prop_collision_postcondition f1 f2 = 
    collision f1 f2 == (let (n1, s1, o1, e1) = limites f1
                            (n2, s2, o2, e2) = limites f2
                        in n1 <= s2 && s1 >= n2 && o1 <= e2 && e1 >= o2)

prop_collision_precondition :: Forme -> Forme -> Bool
prop_collision_precondition f1 f2 = prop_limites_precondition f1 && prop_limites_precondition f2

adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = let (y1, y2, x1, x2) = limites f1
                       (y1', y2', x1', x2') = limites f2
                   in (x2 + 1 == x1' || x1 - 1 == x2') && (y1 <= y2' && y2 >= y1') ||
                      (y2 + 1 == y1' || y1 - 1 == y2') && (x1 <= x2' && x2 >= x1')


-- adjacentes invariant and postcondition
prop_adjacentes_invariant :: Forme -> Forme -> Bool
prop_adjacentes_invariant f1 f2 = 
    adjacentes f1 f2 == adjacentes f2 f1

prop_adjacentes_precondition :: Forme -> Forme -> Bool
prop_adjacentes_precondition f1 f2 = prop_limites_precondition f1 && prop_limites_precondition f2

prop_adjacentes_postcondition :: Forme -> Forme -> Bool
prop_adjacentes_postcondition f1 f2 = 
    adjacentes f1 f2 == 
    (let (n1, s1, o1, e1) = limites f1
         (n2, s2, o2, e2) = limites f2
     in ((n1 == s2 || s1 == n2) && (o1 <= e2 && e1 >= o2)) ||
        ((o1 == e2 || e1 == o2) && (n1 <= s2 && s1 >= n2)))


-- on ne voit pas trop l'intérêt de construire une forme vide 
-- et également de construire mes VSegment et HSegment dans le fonctionnement du jeu
-- donc le joueur pour nous va seulement construire des rectangles et 
-- placer des batiments sur ces zones
smartRectangleConstructer :: Coord -> Int -> Int -> Maybe Forme
smartRectangleConstructer c w h = if w > 0 && h > 0 then Just (Rectangle c w h) else Nothing

-- on pense pas utiliser ces fonctions mais bon ...
smartVSegmentConstructer :: Coord -> Int -> Maybe Forme
smartVSegmentConstructer c l = if l > 0 then Just (VSegment c l) else Nothing

smartHSegmentConstructer :: Coord -> Int -> Maybe Forme
smartHSegmentConstructer c l = if l > 0 then Just (HSegment c l) else Nothing


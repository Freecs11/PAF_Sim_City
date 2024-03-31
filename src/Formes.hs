module Formes where

import GameData

-- formes functions and invariants/pre/post conditions
-- instances also ( functor, applicative, monad, foldable, traversable, etc. )


limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y, y + l - 1, x, x)
limites (Rectangle (C x y) l h) = (y, y + h - 1, x, x + l - 1)


-- invariant test on limites function
testLimites :: Forme -> Bool
testLimites f = let (n, s, o, e) = limites f
                in case f of
                    HSegment (C x y) l -> n == y && s == y && o == x && e == x + l - 1
                    VSegment (C x y) l -> n == y && s == y + l - 1 && o == x && e == x
                    Rectangle (C x y) l h -> n == y && s == y + h - 1 && o == x && e == x + l - 1



appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C x' y') l) = y == y' && x >= x' && x < x' + l
appartient (C x y) (VSegment (C x' y') l) = x == x' && y >= y' && y < y' + l
appartient (C x y) (Rectangle (C x' y') l h) = x >= x' && x < x' + l && y >= y' && y < y' + h

-- invariant test on appartient function
testAppartient :: Coord -> Forme -> Bool
testAppartient c f = appartient c f == (let (n, s, o, e) = limites f
                                        in case f of
                                            HSegment (C x y) l -> y == y && x >= o && x < e
                                            VSegment (C x y) l -> x == x && y >= n && y < s
                                            Rectangle (C x y) l h -> x >= o && x < e && y >= n && y < s)


adjacent :: Coord -> Forme -> Bool
adjacent (C x y) (HSegment (C x' y') l) = (x == x' + l || x == x' - 1) && y >= y' && y < y' + 1
adjacent (C x y) (VSegment (C x' y') l) = (y == y' + l || y == y' - 1) && x >= x' && x < x' + 1
adjacent (C x y) (Rectangle (C x' y') l h) = (x == x' + l || x == x' - 1) && y >= y' && y < y' + h || (y == y' + h || y == y' - 1) && x >= x' && x < x' + l

-- invariant test on adjacent function
testAdjacent :: Coord -> Forme -> Bool
testAdjacent c f = adjacent c f == (let (n, s, o, e) = limites f
                                    in case f of
                                        HSegment (C x y) l -> (x == e || x == o - 1) && y >= n && y < s
                                        VSegment (C x y) l -> (y == s || y == n - 1) && x >= o && x < e
                                        Rectangle (C x y) l h -> (x == e || x == o - 1) && y >= n && y < s || (y == s || y == n - 1) && x >= o && x < e )

collision :: Forme -> Forme -> Bool
collision f1 f2 = let (n1, s1, o1, e1) = limites f1
                      (n2, s2, o2, e2) = limites f2
                  in n1 <= s2 && s1 >= n2 && o1 <= e2 && e1 >= o2



adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = let (n1, s1, o1, e1) = limites f1
                       (n2, s2, o2, e2) = limites f2
                   in (n1 == s2 || s1 == n2) && (o1 == e2 || e1 == o2)

-- SDL SPECIFIC FUNCTIONS
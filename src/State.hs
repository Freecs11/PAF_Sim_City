module State where


-- state of the game (citizens, buildings, zones, etc.) 
-- we'll serve for simulation too

import GameData

import Data.Map (Map)
import qualified Data.Map as Map


data Event = Move Coord CitId -- évenement pour déplacer un citoyen vers un Coord ( normalement celui d'un batiment)
           | StartWork CitId -- commence le travaille , va rajouté dans l'état le temps de fin de travail du citoyen comme currentTime + temps de travail
           -- comme ça si le temps de travail est fini on peut le faire rentrer chez lui en rajoutant un autre évenement GoHome
           | GoShopping CitId -- commence le shopping, meme principe que StartWork
           | GoHome CitId -- rentre chez lui , il signifie que le citoyen a fini son travail ou son shopping et qu'il rentre chez lui directement ( suivant le chemin le plus court)
           | UpdateNeeds CitId -- met à jour les besoins du citoyen ( du genre faim, soif, etc.  qui décroit avec le temps) , donc on va rajouter un évenement pour mettre à jour les besoins de chaque citoyen à chaque tour de boucle
           deriving (Eq, Show)


data Etat =  Etat {
        ville :: Ville,
        coins :: Int,
        carte :: Map Coord (BatId, [CitId]), -- pas bien 
        currentTime :: Int , -- temps actuel du jeu , utilise un entier pour l'instant
        events :: Map Int [Event] 
        -- on va stocker les évenements à faire à un temps donné , 
        -- ce que j'imagine c'est dans la boucle de jeu on va regarder si on a des évenements à faire à currentTime et on les fait
        -- et on les enlève de la liste des évenements
    }
    deriving (Show, Eq)

-- On va utiliser une fonction pour ajouter un évenement à un temps donné
-- Ex : scheduleEvent (getCurrentTime + 1000) (Move (getHomeCoord citoyen) citoyenId) etat 
-- va ajouter un évenement pour déplacer le citoyen à sa maison dans 1000 unités de temps
-- puisque la boucle de jeu va incrémenter le temps de 1 à chaque tour de boucle , 1 unité de temps = 1 tour de boucle = 1 ms ( à peu près je pense )
scheduleEvent :: Int -> Event -> Etat -> Etat
scheduleEvent time event state@(Etat { events = evs }) =
    let updatedEvents = Map.insertWith (++) time [event] evs
    in state { events = updatedEvents }

-- processEvents va être appelé à chaque tour de boucle de jeu et va lancé le traitement des évenements à faire à ce tour
processEvents :: Int -> Etat -> Etat
processEvents tick state@(Etat { events = evs }) =
    case Map.lookup tick evs of
        Just currentEvents -> foldl (flip processEvent) state currentEvents
        Nothing -> state


-- Et c'est ici qu'on va traiter les évenements individuellement
-- processEvent :: Event -> Etat -> Etat
-- processEvent event state = state 
--     case event of
--         Move coord citId -> moveCitizen coord citId state
--         StartWork citId -> startWork citId state
--         GoShopping citId -> goShopping citId state
--         GoHome citId -> goHome citId state
--         UpdateNeeds citId -> updateNeeds citId state
--         _ -> state





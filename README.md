# SimCity en Haskell

Ce projet est une implémentation d'une Simulation du jeu classique SimCity en utilisant le langage Haskell et la bibliothèque graphique SDL2.

## Introduction

Dans le cadre du cours de Programmation Avancée en Style Fonctionnelle (2023-2024) à Sorbonne Université Master 1 STL, ce projet a été développé par moi même Rachid Bouhmad et Do Truong Thinh Truong. Le principal objectif était de créer un jeu vidéo robuste et sécurisé en Haskell, intégrant des techniques telles que le développement dirigé par les tests (TDD), les types algébriques et la bibliothèque graphique SDL2.

## Fonctionnalités

- **Pathfinding avec A*** : Utilisation de l'algorithme A* pour déterminer les trajets les plus courts pour les citoyens.
- **Simulation et gestion des événements** : Simulation réaliste de la vie des citoyens, y compris le travail, le shopping et les déplacements et autres...
- **Gestion des ressources** : Système économique simulant l'imposition et les dépenses des citoyens.
- **Utilisation de SDL2** : Affichage graphique du jeu, incluant la gestion des menus et la création des zones par drag-and-drop.

## Installation

Pour cloner et installer ce projet, assurez-vous d'avoir Haskell et la bibliothèque SDL2 installés sur votre machine.

```bash
# Cloner le dépôt
git clone https://github.com/Freecs11/PAF_Sim_City.git

# Accéder au répertoire du projet
cd PAF_Sim_City

# Installer les dépendances
stack setup
stack install sdl2
stack build
```
## Compilation

Ce projet requiert la bibliothèque sdl2 (Simple Media Libary, v2.0).

Sous Linux ou MacOS, il suffit d'installer la dépendance associée
(par exemple `libsdl2-dev` sous Debian/Ubuntu).

**Remarque**: SDL2 ne semble pas encore compatible avec la puce M1 des nouveaux MAC.

Sous Windows, c'est un peu plus complexe (comme d'habitude).  Le plus simple est de passer par *msys2* dont une version est installée par *stack*.  Normalement, la commande suivante devrait suffire :

```
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2
```

Dans tous les cas, on utilisera :

```
stack build
```

Pour construire le projet.

et :

```
stack run
```

Pour le lancer...

et finalement : 

```
stack test
```

pour lancer les tests Hspec et Quickcheck

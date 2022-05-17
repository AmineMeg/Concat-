Projet de PFA
============================================

**M1 Informatique - Université de Paris**

### Objectif du projet

Concat est un petit langage de programmation qui permet d’accomplir des tˆaches simples de
transformations de chaˆınes de caract`eres. L’objectif de ce projet est de faire de la programmation
par l’exemple : Au lieu de faire ´ecrire un programme Concat par un programmeur, un utilisateur
va donner quelques exemples d’une entr´ee et de la sortie attendue du programme. Puis, le
programme que vous allez ´ecrire va produire un programme Concat qui est coh´erent avec les
exemples fournis (ou une erreur quand un tel programme n’existe pas).


## Setup
Pour build le projet:

`$ ./dune init`

## Utilisation
* Votre programme `genconcat` accepte les modes suivants de fonctionnement :

    - `$ ./dune build projet -- genconcat <fichier>` affiche le programme Concat qui a ete produit à partir du contenu de `<fichier>`.
      Voir le répertoire `projet/exemples` pour des exemples de fichiers à traiter

    - `$ ./dune build projet --genconcat <fichier1> <fichier2>` crée un programme Concat à partir du contenu de <fichier1> (contenant des lignes "input output") puis utiliser ce programme sur <fichier2>. Celui-ci ne contiendra que des lignes "input", et vous devrez afficher chaque "output" calculé correspondant, un par ligne.


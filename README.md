# TypePreuveProjet

Nous avons découvert l'arcane, il faut compléter le fichier typing.ml,
nous devons complété la fonction 'tc_instr' qui vérifie que les
instructions sont typé correctement.


opam install ocaml-lsp-server
opam install menhir
opam install ppx_deriving
opam install ocamlgraph

dune build
dune exec Proj_GraphDB


Le noyau du projet (la plupart dans lib, et main.ml dans bin) :
— display.ml : affichage de graphes
— graphstruct.ml : le type des graphes
— instr.ml : instructions du langage intermédiaire
— interf.ml : interface avec le parser
— lang.ml les types pour manipuler des programmes source
— main.ml programme principal
— sem.ml sémantique / évaluation de programmes.
— typing.ml typage de programmes.
— Analyse lexicale et syntaxique, lexer.mll et parser.mly : Des fichiers pour le développement
d’un lexer/parser avec ocamllex et menhir. Vous devez les compléter et les fournir avec le reste de
votre projet.



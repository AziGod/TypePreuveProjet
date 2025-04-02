# TypePreuveProjet

Nous avons découvert l'arcane, il faut compléter le fichier typing.ml,
nous devons complété la fonction 'tc_instr' qui vérifie que les
instructions sont typé correctement.


opam install ocaml-lsp-server -y
eval $(opam env)    

opam install menhir -y
opam install ppx_deriving -y
opam install ocamlgraph -y
eval $(opam env)


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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Les étapes essentielles du projet sont :

— Mise à jour et extension de l’analyse lexicale et syntaxique du langage MiniGQL, à l’aide des
outils ocamllex et menhir (sect. 3.3).

— Traduction du langage source vers un langage d’instructions plus simple (sect. 3.4).

— Vérification des types du langage source (sect. 3.5). FAIT

— Exécution de ces instructions pour obtenir une réponse à des requêtes (sect. 3.6).

— Combinaison des étapes précédentes pour obtenir un exécutable (sect. 3.7).

— Développement et exécution de tests (sect. 3.8).




MiniGQL permet :
— de construire des graphes, en créant des noeuds et des arcs,
— de les modifier, en supprimant des noeuds ou arcs ou en changeant des attributs
— d’interroger les graphes en cherchant des noeuds qui ont certaines propriétés (valeurs d’attributs
ou relations avec d’autres noeuds)


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

truc qu'on a fait le 24/03 :
- compléter le typing.ml --> tc_instr et check_graph_types (tester sur utop)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
fichier .mly = analyseur syntaxique (parser) = Menhir
fichier .mll = analyseur lexical (lexer) = ocamllex
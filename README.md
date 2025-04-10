# TypePreuveProjet
# Groupe : Thibault MALESPINE / Armand LECOURT

opam install ocaml-lsp-server -y
eval $(opam env)    

opam install menhir -y
opam install ppx_deriving -y
opam install ocamlgraph -y
eval $(opam env)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Nous avons utilisé l'IA dans ce projet pour générer des fonctions Ocaml.

Résumé du travail effectué:
Nous avons complété le projet, il fonctionne.
On peut générer des graphs en pdf avec la table associée à la requête.
Les fichiers suivant ont été complétés:
-parser.mly
-instr.ml
-typing.ml
-sem.ml

Malheureusement, nous n'avons pas eu assez de temps pour approfondir nos tests et en réaliser d'avantage.



Voici la manipulation pour exécuter nos tests:
dune build
dune exec Proj_GraphDB test/entreprise.q
dune exec Proj_GraphDB test/mauvaisTypage.q     #test qu'une relation n'est pas déclarée entre 2 types



Observation/Remarques:
Le projet nous a paru complexe dû à la nouveauté des concepts mais très bien détaillé et guidé pour le réaliser.
Néanmoins, le parser était peu guidé et difficile.

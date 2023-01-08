# Mini-ML

Ici vous trouverez le projet 'Mini-ML' du cours de Compilation LDD3-IM de Gabriel Desfrene.

## Comment compiler le tout ?

Ce projet est construit avec `dune`, ce qui facilite _(grandement)_ la compilation des programmes ``mmlcat`` et ``mmli``. 

Usage :

- Pour tout compiler : ``dune build``
- Pour executer ``mmlcat`` : ``dune exec mmlcat <args>``
- Pour executer ``mmli`` : ``dune exec mmli <args>``
- Pour consulter les fichiers d'erreurs ``menhir`` : Aller dans ``_build/default/``

## Problèmes Rencontrés

Un seul problème a été rencontré, lors de l'implémentation des règles pour le lexer quand aux structures.

La grammaire pour l'instanciation des structures a été modifée en :
```
<s_expr> ::=  n 
            |   true 
            |   false
            |   ()
            |   ident
            |   <s_expr> . ident
            |   { [ident = <s_expr> ;]+ }  (* expr -> s_expr *)
            |   ( <expr> )
```
Afin de traiter les cas de la forme : 

``` let IDENT = { IDENT = CST; IDENT = <expr>; CST; } in <expr>```

En effet, afin de gérer les effets de bords possible lors de l'instanciation d'une strucure on écrira ce cas de la forme :

``` let IDENT = { IDENT = CST; IDENT = (expr; CST); } in <expr>```

## Extentions réalisées

### Inférence des types 
Aucune annotation de type est requise pour le typage du programme. Aucune annotation n'est d'ailleurs possible vu les règles définies dans le parser.
On utilise l'algorithme W _(très largement)_ inspiré de celui proposé dans le cours.

### Argument unit
Les fonctions peuvent être définies (récursivement et ou annonymes ou non) avec l'argument de type unit : ``()``.
Dans ce cas, lors de l'évaluation d'une fonction, aucune variable, n'est rajoutée à l'environement.

On utilise les règles suivantes :
```
expr:
| ...
| fun ( ) -> <expr>
| let <IDENT> ( ) = <expr> in <expr>
| let rec <IDENT> ( ) = <expr> in <expr>
```

### Tableaux homogènes

La définition et l'usage des tableaux est possible dans cette implémentation de 
Mini-ML :

- Définition : `` let t = [| 1; 2; 3; |] ``
- Lecture de la case ``i`` : ``t.(i)``
- Affectation de la case ``i`` : ``t.(i) <- value``

On utilise les règles de grammaire suivantes :
```
expr:
| ...
| <s_expr> . ( <s_expr> ) <- <expr>  (* Écriture *)
;

s_expr:
| ...
| <s_expr> . ( <s_expr> )   (* Lecture *)
| [| [ <IDENT>  = <s_expr> ; ]* |]  (* Définition *)
;
```

###  Égalité Structurelle
Les opérateurs d'égalité et d'inégalité structurels :

- ``( = ) -> a' -> a' -> bool``
- ``( <> ) -> a' -> a' -> bool``

on été implémentés. 

On veillera, dans le cas de structures reccursives, à ne pas boucler infiniment lors du test
(l'égalité des adresses assure l'égalité strucurelle).

### Petites modifications
Quelques peties modifications ont également été realisées :

- Affichage du type du résultat lors de l'interpretation "à la OCaml"
- Quelques modifications au niveau de l'affichage de ``mmlcat`` uniquement dans le but de faciliter le débbugage (l'output reste très moche).

## Tests
Quelques tests ont été ajoutés : 

- ``array.mml`` qui teste le fonctionnement des tableaux
- ``recursive_struct.mml`` qui teste le fonctionnement des structures récursives (On fait des listes !)
- ``polymorphism.mml`` qui teste le polymorphisme d'une fonction.
- ``variable_types.mml`` qui teste l'affichage des types variables.


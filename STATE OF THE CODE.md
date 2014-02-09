State of the Code
================= 

Le code a subi pas mal de modifications par rapport à la version
 originale, pour la plupart concentrées dans ElaborateDictionaries et
 ElaborationEnvironment. Le document présent concerne la version
 actuelle du code (date : 2014-02-09).


Construction globale du programme
---------------------------------

Le point d'entrée, c'est `front.ml`. Pas grand-chose à voir là, si ce
n'est qu'il enchaîne des passes avec un opérateur ($>). Chaque passe
fait 1 truc (parser, élaborer, compiler vers du OCaml) et génère un
résultat intermédiaire, qui se retrouve enregistré dans un fichier.
Pour observer ça, essayer de lancer les tests (cf. README) : le
dossier se retrouve rempli de `.mlse` (AST parsé recraché) et de
`.mlr` (après élaboration, du coreML explicitement typé sans classes)
en plus du `.ml` final.

Le parser, il marche, on s'en fout. De même pour la traduction de
coreML vers OCaml.

La passe `elaborate_dictionaries` fait appel à la fonction `program`
qui se trouve dans `elaboration/elaborateDictionaries.ml`. Ce module
entier est dédié à implémenter cette fonction, qui a 2 rôles :

* Typechecker l'AST (explicitement typé) en entrée
* Recracher en sortie un AST sans typeclasses, en élaborant les
  dictionnaires (d'où le nom)
  
`ElaborateDictionaries` fait à son tour appel aux autres modules
dans `elaboration/`, c'est là que tout se passe !


Fichiers communs dans la racine (`src/`) :
------------------------------------------

`options.ml` on s'en fout. Les autres sont *importants*. Heureusement,
ils sont assez bien documentés.

* `types.ml` : lire le `.mli`, tous les commentaires sont là-dedans.
  Définit la syntaxe des types, schémas etc.
* `AST.ml` : définit l'AST. Truc vicieux : c'est en fait un foncteur,
  qui permet d'avoir la même base pour les AST implicitement et
  explicitement typés. `IAST.ml` et `XAST.ml` ne font qu'appliquer ce
  foncteur à la struct qui va bien, et qui se trouve en fait dans
  `types.ml` ! Donc pour savoir ce qu'est un `binding` (en l'occurrence
  un `name * mltype`) par exemple, il faut fouiller dans `types.ml`.
* `name.ml` : module minuscule qui permet de distinguer les noms de
  variables de noms de types... Les OrderedType sont un ajout perso
  pour pouvoir avoir des Map et Set prenant des noms.

Signalons aussi `common/misc.ml` qui contient des fonctions
utilitaires (ne pas hésiter à en rajouter si besoin ! et fouiller
dedans avant de recoder son propre truc).


Le dossier `elaboration`
------------------------


Qu'est-ce qui est implémenté pour l'instant ?
---------------------------------------------



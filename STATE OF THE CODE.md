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
  Définit la syntaxe des types, schémas etc. Lieu destiné à recevoir
  des fonctions génériques de manipulation de types.
* `AST.ml` : définit l'AST.
  * Truc vicieux : c'est en fait un foncteur, qui permet d'avoir la
    même base pour les AST implicitement et explicitement
    typés. `IAST.ml` et `XAST.ml` ne font qu'appliquer ce foncteur à la
    struct qui va bien, et qui se trouve en fait dans `types.ml` ! Donc
    pour savoir ce qu'est un `binding` (en l'occurrence un `name *
    mltype`) par exemple, il faut fouiller dans `types.ml`,  dans le
    module ExplicitTyping. 
  * Autre truc : tout est pollué par des infos de localisation, pour
    des raisons de error reporting.
* `name.ml` : module minuscule qui permet de distinguer les noms de
  variables de noms de types... Les OrderedType sont un ajout perso
  pour pouvoir avoir des Map et Set prenant des noms.

Signalons aussi `common/misc.ml` qui contient des fonctions
utilitaires (ne pas hésiter à en rajouter si besoin ! et fouiller
dedans avant de recoder son propre truc).


Le dossier `elaboration`
------------------------

* `elaborationErrors.ml` et `elaborationExceptions.ml` : les
  concepteurs du sujet ont été gentils, ils nous ont filé une gestion
  d'erreur qui permet à l'exécutable d'afficher des messages d'erreurs
  lorsqu'on lance une exception durant le typechecking /
  l'élaboration. Voir dans `elaborationExceptions.mli` un listing des
  exceptions possibles + descriptif. Lorsqu'on doit rejeter un
  programme mal typé, lever une de ces exceptions.
* `elaborationEnvironment.ml` : définit un type d'environnement
  et des fonctions pour le manipuler
* `elaborateDictionaries.ml` : le gros truc, là où on doit faire la
  plupart du boulot.

Le boulot de `ElaborateDictionaries.program`, c'est de faire un gros
parcours de l'AST en se trimballant un environnement de typage qu'il
met à jour, et en recrachant au fur et à mesure des bouts du code
élaboré.

### L'environnement

L'environnement est un record qui contient plein de maps. A l'origine
c'était des listes associatives mais ce fut changé. Il sera sans doute
amené à être encore plus modifié si besoin de plus d'informations
locales ; ainsi, à la base, il n'y avait pas de liste d'instances,
et donc pas de moyen de se prémunir contre l'overlapping.

Comme d'hab, cf. le `.mli` pour avoir la documentation des diverses
fonctions qui opèrent sur l'environnement (le type exporté est
abstrait, on est obligé de passer par ces fonctions). Ce que les
commentaires ne mentionnent pas, c'est que les fonctions du genre
lookup et bind se chargent de signaler une erreur pertinente en cas
d'échec (par exemple, `bind_class` soulève l'exception
`AlreadyDefinedClass` si une classe du même nom existe déjà). C'est
pourquoi elles prennent souvent un argument supplémentaire indiquant
la position, pour pouvoir le donner dans l'exception.

### Le module `ElaborateDictionaries`





Qu'est-ce qui est implémenté pour l'instant ?
---------------------------------------------

Examiner les `.mlr` générés par la suite de tests.


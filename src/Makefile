# TODO : 
# Paramétriser la commande d'ocamlbuild
# Virer le reste quand on est sûr qu'on est bons avec ocamlbuild
# -> je le fais, vu que l'historique est dans git...
# Leur clean ne vire pas tous les fichiers du parser (mais on s'en fout, en fait)
# -> en fait si, lexer.ml et parser.ml sont créés dans _build !
# Faire un clean pour virer _build -> c'est fait !

all: build 

build:
	ocamlbuild -Is parsing/pprint-20130324/src,parsing,inference,elaboration,common -use-ocamlfind -use-menhir -yaccflags --explain,--external-tokens,Tokens -cflags -annot front.native && cp front.native joujou

clean:
	rm joujou && ocamlbuild -clean


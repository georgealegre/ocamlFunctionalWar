OCC = ocamlc
OCO = ocamlopt 

all:
	mkdir -p bin; cd src; $(OCC) -c deck.mli player.mli game.mli; $(OCO) -o game deck.ml player.ml game.ml main.ml; mv game ../bin/; mv *.cmi ../bin/; mv *.cmx ../bin/; mv *.o ../bin/

clean :
	rm -R bin; mkdir -p bin

play:
	./bin/game

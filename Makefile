SRC:= $(wildcard *.ml)
EXE:= $(SRC:%.ml=%)

all : $(EXE) clean

% : %.ml
	ocamlc $^ -o $@

clean:
	rm *.cmi
	rm *.cmo
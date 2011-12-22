#This is a Makefile

SB=scanner
PB=parser
AB=ast
SAB=sast
SC=ssc
NC=numlangc

SCANNER= $(SB).mll
PARSER= $(PB).mly
AST= $(AB).ml
SAST= $(SAB).ml

SCANL=$(SB).ml
PARSL=$(PB).ml
PARSI=$(PB).mli

OCL= ocamllex
OCY= ocamlyacc
OCC= ocamlc


OCLOPTS=
OCYOPTS= -v
OCCOPTS= -c
OCCDEBUGOPTS= -c -g

all: $(SCANNER) $(PARSER)
	$(OCL) $(OCLOPTS) $(SCANNER)
	$(OCY) $(OCYOPTS) $(PARSER)
	$(OCC) $(OCCOPTS) $(AST)
	$(OCC) $(OCCOPTS) $(SAST)
	$(OCC) $(OCCOPTS) $(PARSI)
	$(OCC) $(OCCOPTS) $(SCANL)
	$(OCC) $(OCCOPTS) $(PARSL)
	$(OCC) $(OCCOPTS) $(SC).ml
	$(OCC) $(OCCOPTS) $(NC).ml
	$(OCC) -o $(NC) $(PB).cmo $(SB).cmo $(SC).cmo $(NC).cmo $(SAB).cmo

debug: $(SCANNER) $(PARSER)
	$(OCL) $(OCLOPTS) $(SCANNER)
	$(OCY) $(OCYOPTS) $(PARSER)
	$(OCC) $(OCCDEBUGOPTS) $(AST)
	$(OCC) $(OCCDEBUGOPTS) $(SAST)
	$(OCC) $(OCCDEBUGOPTS) $(PARSI)
	$(OCC) $(OCCDEBUGOPTS) $(SCANL)
	$(OCC) $(OCCDEBUGOPTS) $(PARSL)
	$(OCC) $(OCCDEBUGOPTS) $(SC).ml
	$(OCC) $(OCCDEBUGOPTS) $(NC).ml
	$(OCC) -g -o $(NC) $(PB).cmo $(SB).cmo $(SC).cmo $(NC).cmo $(SAB).cmo

clean: $(SCANL) $(PARSL) $(PARSI)
	rm $(SCANL)
	rm $(PARSL)
	rm $(PARSI)
	rm *.cmi
	rm *.cmo
	rm ssc
	rm parser.output

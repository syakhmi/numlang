#This is a Makefile

SB=scanner
PB=parser
AB=ast
SC=ssc

SCANNER= $(SB).mll
PARSER= $(PB).mly
AST= $(AB).ml

SCANL=$(SB).ml
PARSL=$(PB).ml
PARSI=$(PB).mli

OCL= ocamllex
OCY= ocamlyacc
OCC= ocamlc


OCLOPTS=
OCYOPTS= -v
OCCOPTS= -c

all: $(SCANNER) $(PARSER)
	$(OCL) $(OCLOPTS) $(SCANNER)
	$(OCY) $(OCYOPTS) $(PARSER)
	$(OCC) $(OCCOPTS) $(AST)
	$(OCC) $(OCCOPTS) $(PARSI)
	$(OCC) $(OCCOPTS) $(SCANL)
	$(OCC) $(OCCOPTS) $(PARSL)
	$(OCC) $(OCCOPTS) $(SC).ml
	$(OCC) -o $(SC) $(PB).cmo $(SB).cmo $(SC).cmo

clean: $(SCANL) $(PARSL) $(PARSI)
	rm $(SCANL)
	rm $(PARSL)
	rm $(PARSI)
	rm *.cmi
	rm *.cmo
	rm ssc

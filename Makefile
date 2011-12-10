#This is a Makefile

SB=scanner
PB=parser

SCANNER= $(SB).mll
PARSER= $(PB).mly

SCANL=$(SB).ml
PARSL=$(PB).ml
PARSI=$(PB).mli

OCL= ocamllex
OCLOPTS=
OCY= ocamlyacc
OCYOPTS= -v


all: $(SCANNER) $(PARSER)
	$(OCL) $(OCLOPTS) $(SCANNER)
	$(OCY) $(OCYOPTS) $(PARSER)

noopt: $(SCANNER) $(PARSER)
	$(OCL) $(SCANNER)
	$(OCY) $(PARSER)

uninstall: $(SCANL) $(PARSL) $(PARSI)
	rm $(SCANL)
	rm $(PARSL)
	rm $(PARSI)
	

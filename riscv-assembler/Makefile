# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc -g

all: clean riscv

riscv:
	$(COMPILER) -c byte.ml
	$(COMPILER) -c format.ml
	$(COMPILER) -c riscv_ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c riscv_assem.ml
	$(COMPILER) -c riscv_sim.ml
	$(COMPILER) -c riscv.ml
	$(COMPILER) -o ps1 byte.cmo format.cmo riscv_ast.cmo parse.cmo lex.cmo riscv_assem.cmo riscv_sim.cmo riscv.cmo

clean:
	-rm *.cmo *.cmi ps1 parse.ml parse.mli lex.ml 

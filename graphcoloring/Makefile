# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc

all: clean cish cfg

riscv:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c riscv.ml

cish: riscv
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml

cfg: riscv cish
	$(COMPILER) -c cish_ast.ml
	$(COMPILER) -c cfg_ast.ml
	$(COMPILER) -c spill.ml
	$(COMPILER) -c graph.ml
	$(COMPILER) -c cfg.ml
	$(COMPILER) -c cfg_compile.ml
	$(COMPILER) -c cish_compile.ml
	$(COMPILER) -c main.ml
	$(COMPILER) -o proj cish_ast.cmo cish_lex.cmo cish_parse.cmo word32.cmo riscv.cmo cfg_ast.cmo graph.cmo spill.cmo cfg.cmo cfg_compile.ml cish_compile.cmo main.cmo

clean:
	-rm *.cmo *.cmi proj cish_parse.ml cish_parse.mli cish_lex.ml 

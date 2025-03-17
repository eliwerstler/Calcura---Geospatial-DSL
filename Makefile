all: create_out_dir
	ocamlopt -c -o out/ast.cmx src/ast.ml
	ocamlopt -c -I out -o out/lexer.cmx src/lexer.ml
	ocamlopt -c -I out -o out/parser.cmx src/parser.ml
	ocamlopt -c -I out -o out/interpreter.cmx src/interpreter.ml
	ocamlopt -c -I out -o out/main.cmx src/main.ml
	ocamlopt -o calcura out/ast.cmx out/lexer.cmx out/parser.cmx out/interpreter.cmx out/main.cmx

test: create_out_dir
	ocamlopt -c -o out/ast.cmx src/ast.ml
	ocamlopt -c -I out -o out/lexer.cmx src/lexer.ml
	ocamlopt -c -I out -o out/parser.cmx src/parser.ml
	ocamlopt -c -I out -o out/interpreter.cmx src/interpreter.ml
	ocamlopt -c -I out -o out/test.cmx test/test.ml
	ocamlopt -o test.exe out/ast.cmx out/lexer.cmx out/parser.cmx out/interpreter.cmx out/test.cmx
	test.exe

create_out_dir:
	if not exist out mkdir out

clean:
	if exist out\*.cmi del /Q out\*.cmi
	if exist out\*.cmx del /Q out\*.cmx
	if exist out\*.o del /Q out\*.o
	if exist calcura.exe del /Q calcura.exe
	if exist test.exe del /Q test.exe

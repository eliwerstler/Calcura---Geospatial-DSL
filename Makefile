compile_sources: create_out_dir
	ocamlopt -c -o out/ast.cmx src/ast.ml
	ocamlopt -c -I out -o out/scanner.cmx src/scanner.ml
	ocamlopt -c -I out -o out/parser.cmx src/parser.ml
	ocamlopt -c -I out -o out/interpreter.cmx src/interpreter.ml

all: compile_sources
	ocamlopt -c -I out -o out/main.cmx src/main.ml
	ocamlopt -o calcura out/ast.cmx out/scanner.cmx out/parser.cmx out/interpreter.cmx out/main.cmx

test: compile_sources
	ocamlopt -c -I out -o out/test_suite.cmx test/test_suite.ml
	ocamlopt -o test_suite.exe out/ast.cmx out/scanner.cmx out/parser.cmx out/interpreter.cmx out/test_suite.cmx
	./test_suite.exe
	
create_out_dir:
	@if not exist out (mkdir out)

clean:
	@if exist out\*.cmi ( \
		if exist NUL (del /Q out\*.cmi) else (rm -f out/*.cmi) \
	)
	@if exist out\*.cmx ( \
		if exist NUL (del /Q out\*.cmx) else (rm -f out/*.cmx) \
	)
	@if exist out\*.o ( \
		if exist NUL (del /Q out\*.o) else (rm -f out/*.o) \
	)
	@if exist calcura.exe ( \
		if exist NUL (del /Q calcura.exe) else (rm -f calcura.exe) \
	)
	@if exist test_suite.exe ( \
		if exist NUL (del /Q test_suite.exe) else (rm -f test_suite.exe) \
	)

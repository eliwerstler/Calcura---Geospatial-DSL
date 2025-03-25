compile_sources: create_out_dir
	ocamlopt -c -o out/ast.cmx src/ast.ml
	ocamlopt -c -I out -o out/scanner.cmx src/scanner.ml
	ocamlopt -c -I out -o out/parser.cmx src/parser.ml
	ocamlopt -c -I out -o out/interpreter.cmx src/interpreter.ml

all: compile_sources
	ocamlopt -c -I out -o out/main.cmx src/main.ml
	ocamlopt -o out/calcura out/ast.cmx out/scanner.cmx out/parser.cmx out/interpreter.cmx out/main.cmx

test: compile_sources
	ocamlopt -c -I +str -I +unix -I out -o out/test_suite.cmx test/test_suite.ml
	ocamlopt -I +str -I +unix -o out/test_suite.exe str.cmxa unix.cmxa out/ast.cmx out/scanner.cmx out/parser.cmx out/interpreter.cmx out/test_suite.cmx
	./out/test_suite.exe

# Run a single test: make test-single TEST=<test_file>
test-single: compile_sources
	ocamlopt -c -I +str -I +unix -I out -o out/test_suite.cmx test/test_suite.ml
	ocamlopt -I +str -I +unix -o out/test_suite.exe str.cmxa unix.cmxa out/ast.cmx out/scanner.cmx out/parser.cmx out/interpreter.cmx out/test_suite.cmx
	./out/test_suite.exe $(TEST)
	
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
	@if exist out\*.exe ( \
		if exist NUL (del /Q out\*.exe) else (rm -f out/*.exe) \
	)
	@if exist test\out\*.out ( \
		if exist NUL (del /Q test\out\*.out) else (rm -f test/out/*.out) \
	)

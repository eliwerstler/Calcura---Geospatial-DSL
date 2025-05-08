# Makefile - Cross-platform (macOS, Linux, Git Bash on Windows)

OUT_DIR = out
SRC_DIR = src
TEST_DIR = test
SOURCES = $(SRC_DIR)/ast.ml $(SRC_DIR)/scanner.ml $(SRC_DIR)/parser.ml $(SRC_DIR)/interpreter.ml
OBJECTS = $(OUT_DIR)/ast.cmx $(OUT_DIR)/scanner.cmx $(OUT_DIR)/parser.cmx $(OUT_DIR)/interpreter.cmx
MAIN_OBJ = $(OUT_DIR)/main.cmx
EXECUTABLE = $(OUT_DIR)/calcura
TEST_EXEC = $(OUT_DIR)/test_suite.exe

.PHONY: all compile_sources test test-single clean create_out_dir repl

all: compile_sources
	ocamlopt -c -I $(OUT_DIR) -o $(MAIN_OBJ) $(SRC_DIR)/main.ml
	ocamlopt -o $(EXECUTABLE) $(OBJECTS) $(MAIN_OBJ)

compile_sources: create_out_dir
	ocamlc -c -o $(OUT_DIR)/ast.cmo $(SRC_DIR)/ast.ml
	ocamlc -c -I $(OUT_DIR) -o $(OUT_DIR)/scanner.cmo $(SRC_DIR)/scanner.ml
	ocamlc -c -I $(OUT_DIR) -o $(OUT_DIR)/parser.cmo $(SRC_DIR)/parser.ml
	ocamlc -c -I $(OUT_DIR) -o $(OUT_DIR)/interpreter.cmo $(SRC_DIR)/interpreter.ml
	ocamlopt -c -o $(OUT_DIR)/ast.cmx $(SRC_DIR)/ast.ml
	ocamlopt -c -I $(OUT_DIR) -o $(OUT_DIR)/scanner.cmx $(SRC_DIR)/scanner.ml
	ocamlopt -c -I $(OUT_DIR) -o $(OUT_DIR)/parser.cmx $(SRC_DIR)/parser.ml
	ocamlopt -c -I $(OUT_DIR) -o $(OUT_DIR)/interpreter.cmx $(SRC_DIR)/interpreter.ml

test: compile_sources
	ocamlopt -c -I +str -I +unix -I $(OUT_DIR) -o $(OUT_DIR)/test_suite.cmx $(TEST_DIR)/test_suite.ml
	ocamlopt -I +str -I +unix -o $(TEST_EXEC) str.cmxa unix.cmxa $(OBJECTS) $(OUT_DIR)/test_suite.cmx
	./$(TEST_EXEC)

# Run a single test: make test-single TEST=<test_file>
test-single: compile_sources
	ocamlopt -c -I +str -I +unix -I $(OUT_DIR) -o $(OUT_DIR)/test_suite.cmx $(TEST_DIR)/test_suite.ml
	ocamlopt -I +str -I +unix -o $(TEST_EXEC) str.cmxa unix.cmxa $(OBJECTS) $(OUT_DIR)/test_suite.cmx
	./$(TEST_EXEC) $(TEST)

repl: compile_sources
	echo '#use "src/interpreter.ml";;' > .ocamlinit
	ocaml \
		-I $(OUT_DIR) \
		$(OUT_DIR)/ast.cmo \
		$(OUT_DIR)/scanner.cmo \
		$(OUT_DIR)/parser.cmo \
		$(OUT_DIR)/interpreter.cmo

create_out_dir:
ifeq ($(OS),Windows_NT)
	@if not exist $(OUT_DIR) mkdir $(OUT_DIR)
	@if not exist $(TEST_DIR)\out mkdir $(TEST_DIR)\out
else
	@mkdir -p $(OUT_DIR) $(TEST_DIR)/out
endif

clean:
ifeq ($(OS),Windows_NT)
	@if exist $(OUT_DIR) del /Q $(OUT_DIR)\*.cmi $(OUT_DIR)\*.cmx $(OUT_DIR)\*.o $(OUT_DIR)\*.exe $(OUT_DIR)\*~ $(TEST_DIR)\out\*.out
else
	@rm -f $(OUT_DIR)/*.cmi $(OUT_DIR)/*.cmx $(OUT_DIR)/*.o $(OUT_DIR)/*.exe $(OUT_DIR)/*~ $(TEST_DIR)/out/*.out
endif
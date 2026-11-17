# Calcura

**A small programming language for geospatial math, with its own scanner, parser, AST, and interpreter, written in OCaml.**

```c
let new_york = (40.7128, -74.0060);
let london   = (51.5074, -0.1278);

func km_to_miles(km) {
  return km * 0.621371;
}

print(km_to_miles(haversine(new_york, london)));   // about 3461
```

Calcura has variables, conditionals, loops, lists, user-defined functions, and built-in geospatial and statistical operations (haversine distance, coordinate shifts, sort, median, min/max). A file-driven test suite checks every `print` against an expected value written in the comment above it.

**My role:** I added user-defined functions end to end: the `func` syntax in the scanner and parser, the AST nodes, call and return handling with a local scope in the interpreter, and the tests for them.

Team project for Columbia's COMS W4115 (Programming Languages & Translators).

---


Calcura is a lightweight, domain-specific programming language designed for geospatial analysis and spatial computations. It provides intuitive syntax and optimized operations for handling geographical data, making it ideal for map processing, space exploration analytics, and geospatial computations.

### File Extension
Calcura source code files use the `.clc` file extension.

### Team Members:
- Sayef Iqbal
- Crystal Martinez
- Junqi Zhang
- Stanley Lin
- Tyler Andrews
- Eli M Werstler

### Development Setup Instructions

#### Prerequisites
- Install VS Code
    - Install "OCaml Platform" Extension
- Install OCaml
    - https://ocaml.org/docs/installing-ocaml

#### Initial Setup First-Time (Command Prompt)
```
opam init
opam install ocaml-lsp-server odoc ocamlformat utop

// on windows cmd
for /f "tokens=*" %i in ('opam env') do @%i

// OR

// on linux/mac
eval $(opam env)
```

#### New Dev Session
```
opam init

// on windows cmd
for /f "tokens=*" %i in ('opam env') do @%i

// OR

// on linux/mac
eval $(opam env)
```

#### Make Commands
```
make
make clean
make test
make test-single TEST=test/unit_test/test_variable_declaration.clc
```

#### Unit Test Suite 
- make test will default to running all tests in the unit_test directory. 
- Every test file in this directory must contain a comment above each print statement that states the expected value for that print. This is how the test suite knows whether a test is passing or failing. Without this comment any newly added test will fail.
- To run a single test on its own use make test-single TEST=TESTPATH
e.g. make test-single TEST=test/unit_test/test_variable_declaration.clc
- test output is written to test/out/test_results.txt, analyze this file for any failures


#### Example Dev Flow
- Start session
```
opam init
for /f "tokens=*" %i in ('opam env') do @%i
```
- Update parser to add functionality
```
make clean
make test
```
- Ensure all unit tests are passing
- Update unit test to test new functionality
```
make clean
make test
```
- Ensure all unit tests are passing
- Commit/push/pull request
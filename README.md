# Calcura
## COMS W4115 Team Project

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

#### Initial Setup First-Time (Command Prompt)
```
opam init
opam install ocaml-lsp-server odoc ocamlformat utop
for /f "tokens=*" %i in ('opam env') do @%i
```

#### New Dev Session
```
opam init
for /f "tokens=*" %i in ('opam env') do @%i
```

#### Make Commands
```
make
make clean
make test
make test-single TEST=test/unit_test/test_variable_declaration.clc
```

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
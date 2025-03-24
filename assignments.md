# Lexer (Tokenization)
## Assignee: ?

Implement the Lexer module to tokenize the input source code into a stream of tokens.
Define token types (e.g., keywords, identifiers, operators, literals).
Write functions to handle edge cases like invalid tokens or whitespace handling.
Example tasks:
Implement tokenize function.
Define token types in a tokens.ml file.

# Parser (Syntax Analysis)
## Assignee: ?

Implement the Parser module to convert the token stream into an Abstract Syntax Tree (AST).
Define grammar rules for the language.
Handle syntax errors gracefully.
Example tasks:
Implement parse function.
Define the grammar and AST structure in parser.ml.

# AST and Semantic Analysis
## Assignee: Sayef

Define the Abstract Syntax Tree (AST) structure in a separate module (e.g., ast.ml).
Implement semantic analysis to check for errors like type mismatches or undefined variables.
Example tasks:
Define AST types (e.g., expr, stmt, program).
Implement semantic checks in semantic.ml.

# Interpreter (Execution)
## Assignee: ?

Implement the Interpreter module to execute the AST.
Handle runtime errors (e.g., division by zero, invalid operations).
Example tasks:
Implement run function to evaluate the AST.
Handle variable storage and scope (e.g., symbol table).

# Standard Library and Built-in Functions
## Assignee: ?

Implement built-in functions (e.g., print, input, mathematical operations).
Define a standard library for common utilities.
Example tasks:
Implement built-in functions in stdlib.ml.
Integrate the standard library with the interpreter.

# Testing and Debugging
## Assignee: Tyler, Crystal

Write comprehensive test cases for all components (lexer, parser, interpreter).
Create a test suite in test.ml to automate testing.
Debug and ensure all components work together seamlessly.
Example tasks:
Add test cases for valid and invalid inputs.
Write scripts to automate testing.
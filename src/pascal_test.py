from pascal_exemplos import *
from pascal_analex import tokens, literals, lexer
from pascal_anasin import parser
import sys

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "ast":
        printAst = True
    for i in "0123456789":
        texto = exemplos[i]
        lexer.has_errors = False
        lexer.input(texto)
        for tok in lexer:  # Força a análise de todos os tokens
            pass
        if lexer.has_errors:
            print(f"\033[1;31mExample {i} - Lexical analysis  ❌\033[0m")
        else:
            print(f"\033[1;92mExample {i} - Lexical analysis  ✅\033[0m")
    asts = []
    for i in "0123456789":
        texto = exemplos[i]
        parser.has_errors = False
        ast = parser.parse(texto)
        if parser.has_errors:
            print(f"\033[1;31mExample {i} - Syntax analysis   ❌\033[0m")
            asts.append(None)
        else:
            print(f"\033[1;92mExample {i} - Syntax analysis   ✅\033[0m")
            asts.append(ast)
    for i in "0123456789":
        has_error = asts[int(i)].anasem()
        if has_error:
            print(f"\033[1;31mExample {i} - Semantic analysis ❌\033[0m")
        else:
            print(f"\033[1;92mExample {i} - Semantic analysis ✅\033[0m")

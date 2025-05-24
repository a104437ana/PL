from pascal_anasin import parser
from pascal_analex import tokens, literals, lexer
from run_code import runCode
import asyncio
import sys
import os

if __name__ == "__main__":
    cwd = os.getcwd()
    if len(sys.argv) == 3 or len(sys.argv) == 4:
        f_in = open(f"{cwd}/{sys.argv[1]}", 'r')
        lines = f_in.readlines()
        codigo = "".join(lines)
        f_in.close()
    elif len(sys.argv) == 1:
        codigo = input()
    else:
        print("Incorrect arguments")
        print("py pascal_compiler.py")
        print("py pascal_compiler.py <input file> <output file>")
        print("py pascal_compiler.py <input file> -vm")
        sys.exit()

    lexer.has_errors = False
    lexer.input(codigo)
    for tok in lexer:
        pass
    if lexer.has_errors:
        print(f"\033[1;31mLexical analysis ❌\033[0m")
    else:
        print(f"\033[1;92mLexical analysis ✅\033[0m")

    parser.has_errors = False
    ast = parser.parse(codigo)
    if parser.has_errors:
        print(f"\033[1;31mSyntax analysis  ❌\033[0m")
    else:
        print(f"\033[1;92mSyntax analysis  ✅\033[0m")
    
    if len(sys.argv) == 3 and sys.argv[2] != "-vm":
        if os.path.isdir(f"{cwd}/out"):
            out_dir = f"{cwd}/out"
        elif os.path.isdir(f"{cwd}/../out"):
            out_dir = f"{cwd}/../out"
        f_out = open(f"{out_dir}/{sys.argv[2]}", 'w')
        f_out.writelines(str(ast)) # alterar para função que gera código
        f_out.close()
    elif len(sys.argv) == 1:
        print(ast) # alterar para função que gera código
    elif len(sys.argv) == 3 and sys.argv[2] == "-vm":
        code = ast.generateVmCode()
        asyncio.run(runCode(code))
    else:
        print("Incorrect arguments")
        sys.exit()

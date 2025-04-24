import ply.yacc as yacc
from pascal_analex import tokens, literals
from pascal_exemplos import *

def p_programa(p):
    "programa : BEGIN bloco END '.'"
    print("Programa válido!")

def p_bloco(p):
    'bloco : '
    print("Bloco válido!")

# Tratando erros de sintaxe
def p_error(p):
    print(f"Erro sintático - {p}")

parser = yacc.yacc()

r = parser.parse(exemplo0)
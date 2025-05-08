import ply.yacc as yacc
from ply.yacc import YaccProduction
from pascal_analex import tokens, literals
from pascal_exemplos import *

"""
Gramática:

P1                program : PROGRAM ID ';' variables_declaration main_block '.'
P2  variables_declaration : VAR variables_list
P3                        |
P4         variables_list : same_type_variables
P5                        | variables_list same_type_variables
P6    same_type_variables : ID id_list ':' ID ';'
P7                id_list : id_list ',' ID
P8                        |
P9             main_block : BEGIN END
"""

def p_program(p: YaccProduction):
    "program : PROGRAM ID ';' variables_declaration main_block '.'"
    print(f"Reconheci P1: {p.slice}")

def p_variables_declaration(p: YaccProduction):
    """variables_declaration : VAR variables_list
                             |"""
    if len(p) == 3:
        print(f"Reconheci P2: {p.slice}")
    elif len(p) == 1:
        print(f"Reconheci P3: {p.slice}")

def p_variables_list(p: YaccProduction):
    """variables_list : same_type_variables
                      | variables_list same_type_variables"""
    if len(p) == 2:
        print(f"Reconheci P4: {p.slice}")
    elif len(p) == 3:
        print(f"Reconheci P5: {p.slice}")

def p_same_type_variables(p: YaccProduction):
    "same_type_variables : ID id_list ':' ID ';'"
    print(f"Reconheci P6: {p.slice}")

def p_id_list(p: YaccProduction):
    """id_list : id_list ',' ID
               |"""
    if len(p) == 4:
        print(f"Reconheci P7: {p.slice}")
    elif len(p) == 1:
        print(f"Reconheci P8: {p.slice}")

def p_main_block(p: YaccProduction):
    "main_block : BEGIN END"
    print(f"Reconheci P9: {p.slice}")

# Tratando erros de sintaxe
def p_error(p: YaccProduction):
    print(f"Erro sintático - {p}")

parser = yacc.yacc()

r = parser.parse(exemplo8)
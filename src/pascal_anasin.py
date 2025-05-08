import ply.yacc as yacc
from ply.yacc import YaccProduction
from pascal_analex import tokens, literals
from pascal_exemplos import *

def p_program(p):
    "program : PROGRAM ID ';' variables_declaration main_block '.'"
    print(f"Reconheci P1: {p.slice}")

def p_variables_declaration(p):
    """variables_declaration : VAR variables_list
                             |"""
    print(f"Reconheci P2: {p.slice}")

def p_variables_list(p):
    """variables_list : same_type_variables
                      | same_type_variables variables_list"""
    print(f"Reconheci P3: {p.slice}")

def p_same_type_variables(p):
    "same_type_variables : ID id_list ':' ID ';'"
    print(f"Reconheci P4: {p.slice}")

def p_id_list(p):
    """id_list : ',' ID id_list
               |"""
    print(f"Reconheci P5: {p.slice}")

def p_main_block(p: YaccProduction):
    "main_block : BEGIN END"
    print(f"Reconheci P6: {p.slice}")

# Tratando erros de sintaxe
def p_error(p):
    print(f"Erro sintático - {p}")

parser = yacc.yacc()

r = parser.parse(exemplo8)
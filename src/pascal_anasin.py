import ply.yacc as yacc
from ply.yacc import YaccProduction
from pascal_analex import tokens, literals
from pascal_exemplos import *

"""
Gramática:

P1                program : PROGRAM ID ';' variables_declaration code_block '.'
P2  variables_declaration : VAR variables_list
P3                        |
P4         variables_list : same_type_variables
P5                        | variables_list same_type_variables
P6    same_type_variables : ID id_list ':' ID ';'
P7                id_list : id_list ',' ID
P8                        |
P9             code_block : BEGIN algorithm END
P10             algorithm : assignment algorithm
P11                       | conditional algorithm
P12                       | loop algorithm
P13                       |
P14            assignment : ID '=' assignment_value ';'
P15      assignment_value : value
P16                       | exp
P17                 value : ID
P18                       | INT
P19                       | REAL
P20                       | STRING
P21           conditional : if ELSE conditional
P22                       | if else
P23                    if : IF cond THEN algorithm
P24                  else : ELSE algorithm
P25                  loop : for
P26                       | while
P27                   for : FOR for_cond DO code_block ';'
P28              for_cond : '(' cond ')'
P39                 while : WHILE cond DO code_block ';'
P30            while_cond : '(' cond ')'
P31                   exp : STRING
P32                  cond : STRING
"""

def p_program(p: YaccProduction):
    "program : PROGRAM ID ';' variables_declaration code_block '.'"
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

def p_code_block(p: YaccProduction):
    "code_block : BEGIN algorithm END"
    print(f"Reconheci P9: {p.slice}")

def p_algorithm(p: YaccProduction):
    """algorithm : assignment algorithm
                 | conditional algorithm
                 | loop algorithm
                 |"""
    if len(p) == 3:
        if p.slice[1].type == "assignment":
            print(f"Reconheci P10: {p.slice}")
        elif p.slice[1].type == "conditional":
            print(f"Reconheci P11: {p.slice}")
        elif p.slice[1].type == "loop":
            print(f"Reconheci P12: {p.slice}")
    elif len(p) == 1:
        print(f"Reconheci P13: {p.slice}")

def p_assignment(p: YaccProduction):
    "assignment : ID ASSIGNMENT assignment_value ';'"
    print(f"Reconheci P14: {p.slice}")

def p_assignment_value(p: YaccProduction):
    """assignment_value : value
                        | exp"""
    if p.slice[1].type == "value":
        print(f"Reconheci P15: {p.slice}")
    elif p.slice[1].type == "exp":
        print(f"Reconheci P16: {p.slice}")

def p_value(p: YaccProduction):
    """value : ID
             | INT
             | REAL
             | STRING"""
    if p.slice[1].type == "ID":
        print(f"Reconheci P17: {p.slice}")
    elif p.slice[1].type == "INT":
        print(f"Reconheci P18: {p.slice}")
    elif p.slice[1].type == "REAL":
        print(f"Reconheci P19: {p.slice}")
    elif p.slice[1].type == "STRING":
        print(f"Reconheci P20: {p.slice}")

def p_conditional(p: YaccProduction):
    """conditional : if ELSE conditional
                   | if else"""
    if p.slice[2].type == "ELSE": # não funciona direito com "else if"
        print(f"Reconheci P21: {p.slice}")
    if p.slice[2].type == "else":
        print(f"Reconheci P22: {p.slice}")

def p_if(p: YaccProduction):
    "if : IF '(' cond ')' THEN algorithm"
    print(f"Reconheci P23: {p.slice}")

def p_else(p: YaccProduction):
    "else : ELSE algorithm"
    print(f"Reconheci P24: {p.slice}")

def p_loop(p: YaccProduction):
    """loop : for
            | while"""
    if p.slice[1].type == "for":
        print(f"Reconheci P25: {p.slice}")
    elif p.slice[1].type == "while":
        print(f"Reconheci P26: {p.slice}")

def p_for(p: YaccProduction):
    "for : FOR for_cond DO code_block ';'"
    print(f"Reconheci P27: {p.slice}")

def p_for_cond(p: YaccProduction):
    "for_cond : '(' cond ')'"
    print(f"Reconheci P28: {p.slice}") # incompleto

def p_while(p: YaccProduction):
    "while : WHILE while_cond DO code_block ';'"
    print(f"Reconheci P29: {p.slice}")

def p_while_cond(p: YaccProduction):
    "while_cond : '(' cond ')'"
    print(f"Reconheci P30: {p.slice}") # incompleto

def p_exp(p: YaccProduction):
    "exp : ID"
    print(f"Reconheci P31: {p.slice}") # incompleto

def p_cond(p: YaccProduction):
    "cond : ID"
    print(f"Reconheci P32: {p.slice}") # incompleto

# Tratando erros de sintaxe
def p_error(p: YaccProduction):
    print(f"Erro sintático - {p}")

parser = yacc.yacc()

r = parser.parse(exemplo8)
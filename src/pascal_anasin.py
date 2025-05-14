import sys
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
    """program : PROGRAM ID ';' variables_declaration code_block '.'
               | code_block '.'"""
    #print(f"Recognized P1: {p}")

def p_variables_declaration(p: YaccProduction):
    """variables_declaration : VAR variables_list
                             |"""
    #if len(p) == 3:
        #print(f"Recognized P2")
    #elif len(p) == 1:
        #print(f"Recognized P3")

def p_variables_list(p: YaccProduction):
    """variables_list : same_type_variables
                      | variables_list same_type_variables"""
    #if len(p) == 2:
        #print(f"Recognized P4")
    #elif len(p) == 3:
        #print(f"Recognized P5")

def p_same_type_variables(p: YaccProduction):
    """same_type_variables : ID id_list ':' DATATYPE ';'
                           | ID id_list ':' ARRAY '[' INT RANGE INT ']' OF DATATYPE ';' """
    #print(f"Recognized P6")

def p_id_list(p: YaccProduction):
    """id_list : id_list ',' ID
               |"""
    #if len(p) == 4:
        #print(f"Recognized P7")
    #elif len(p) == 1:
        #print(f"Recognized P8")

def p_code_block(p: YaccProduction):
    "code_block : BEGIN algorithm END"
    #print(f"Recognized P9")

def p_algorithm(p: YaccProduction):
    """algorithm : assignment ';' algorithm
                 | assignment
                 | if ';' algorithm
                 | if
                 | if_else ';' algorithm
                 | if_else
                 | func_call ';' algorithm
                 | func_call
                 | loop ';' algorithm
                 | loop 
                 | """
    #if len(p) == 3:
        #if p.stack[1].type == "assignment":
            #print(f"Recognized P10")
        #elif p.stack[1].type == "conditional":
            #print(f"Recognized P11")
        #elif p.stack[1].type == "loop":
            #print(f"Recognized P12")
    #elif len(p) == 1:
        #print(f"Recognized P13")

def p_assignment(p: YaccProduction):
    "assignment : ID ASSIGNMENT assignment_value"
    #print(f"Recognized P14")

def p_assignment_value(p: YaccProduction):
    """assignment_value : value
                        | expr"""
    #if p.stack[1].type == "value":
        #print(f"Recognized P15")
    #elif p.stack[1].type == "exp":
        #print(f"Recognized P16")

def p_value(p: YaccProduction):
    """value : ID
             | INT
             | REAL
             | STRING
             | BOOL
             | ID '[' INT ']' 
             | ID '[' ID ']'"""
    #if p.stack[1].type == "ID":
        #print(f"Recognized P17")
    #elif p.stack[1].type == "INT":
        #print(f"Recognized P18")
    #elif p.stack[1].type == "REAL":
        #print(f"Recognized P19")
    #elif p.stack[1].type == "STRING":
        #print(f"Recognized P20")

def p_if_else(p):
    '''if_else : IF cond THEN if_else ELSE if_else
               | assignment
               | func_call'''

def p_if(p):
    '''if : IF cond THEN if_else ELSE if 
          | IF cond THEN algorithm'''

def p_loop(p: YaccProduction):
    """loop : for
            | while"""
    #if p.stack[1].type == "for":
        #print(f"Recognized P25")
    #elif p.stack[1].type == "while":
        #print(f"Recognized P26")

def p_for(p: YaccProduction):
    '''for : FOR for_cond DO code_block
           | FOR for_cond DO algorithm'''
    #print(f"Recognized P27")

def p_for_cond(p: YaccProduction):
    '''for_cond : cond 
                | assignment TO ID
                | assignment TO INT
                | assignment DOWNTO ID
                | assignment DOWNTO INT'''
    #print(f"Recognized P28") # incompleto

def p_while(p: YaccProduction):
    '''while : WHILE while_cond DO code_block
             | WHILE while_cond DO algorithm'''
    #print(f"Recognized P29")

def p_while_cond(p: YaccProduction):
    '''while_cond : cond'''
    #print(f"Recognized P30") # incompleto

def p_cond(p: YaccProduction):
    '''cond : expr
            | expr op_rel expr'''

def p_op_rel(p: YaccProduction):
    '''op_rel : '='
              | NOT_EQUAL
              | '<'
              | LESS_EQUAL
              | '>'
              | GREATER_EQUAL'''

def p_expr(p: YaccProduction):
    '''expr : termo
            | expr op_ad termo'''

def p_termo(p: YaccProduction):
    '''termo : fator
             | termo op_mul fator '''

def p_op_ad(p: YaccProduction):
    '''op_ad : '+'
             | '-'
             | OR'''

def p_op_mul(p: YaccProduction):
    '''op_mul : '*'
             | '/'
             | AND
             | MOD
             | DIV'''

def p_fator(p: YaccProduction):
    '''fator : value
             | func_call
             | '(' cond ')' '''

def p_func_call(p: YaccProduction):
    "func_call : ID '(' args ')'"

def p_args(p: YaccProduction):
    '''args : elems
            |'''

def p_elems(p: YaccProduction):
    '''elems : elems ',' value
             | value'''

# Tratando erros de sintaxe
def p_error(p: YaccProduction):
    if p:
        print(f"\033[1;31mSyntax error {p}\033[0m")
    else:
        print(f"\033[1;31mSyntax error at EOF\033[0m")
    parser.has_errors = True

parser = yacc.yacc()
parser.has_errors = False

if __name__ == "__main__":
    texto = ""
    if len(sys.argv) < 2:
        texto = input()
    else:
        escolha = sys.argv[1]
        if escolha in exemplos:
            print(exemplos[escolha])
            texto = exemplos[escolha]
    if texto != "":
        print("\033[1;93mSyntax analysis:\033[0m")
        r = parser.parse(texto,debug=True)
        if parser.has_errors:
            print("\033[1;31mSyntax errors were found ❌\033[0m")
        else:
            print("\033[1;92mSyntax analysis completed without errors ✅\033[0m")
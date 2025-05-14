import sys
import ply.yacc as yacc
from ply.yacc import YaccProduction
from pascal_analex import tokens, literals
from pascal_exemplos import *

"""
program : PROGRAM ID ';' functions variables_declaration code_block '.'
        | code_block '.'

functions : functions function
                 | function
                 |

function : FUNCTION ID '(' parameters ')' ':' DATATYPE ';' variables_declaration code_block ';'

parameters : parameter_list
            |

parameter_list : parameter_list ';' parameter
                | parameter

parameter : ID ':' DATATYPE

variables_declaration : VAR variables_list
                        |

variables_list : same_type_variables
| variables_list same_type_variables

same_type_variables : ID id_list ':' DATATYPE ';'
| ID id_list ':' ARRAY '[' INT RANGE INT ']' OF DATATYPE ';' 

id_list : id_list ',' ID
|

algorithm : assignment ';' algorithm
                 | assignment
                 | if ';' algorithm
                 | if
                 | if_else ';' algorithm
                 | if_else
                 | func_call ';' algorithm
                 | func_call
                 | loop ';' algorithm
                 | loop
| 

assignment : ID ASSIGNMENT assignment_value

assignment_value : value
| expr

value : ID
             | INT
             | REAL
             | STRING
             | BOOL
             | ID '[' INT ']'
| ID '[' ID ']'

if_else : IF cond THEN algorithm ELSE algorithm
               | assignment
| func_call

if : IF cond THEN algorithm

loop : for
| while

for : FOR for_cond DO code_block
| FOR for_cond DO algorithm

for_cond : cond
                | assignment TO ID
                | assignment TO INT
                | assignment DOWNTO ID
| assignment DOWNTO INT

while : WHILE while_cond DO code_block
| WHILE while_cond DO algorithm

while_cond : cond

cond : expr
| expr op_rel expr

op_rel : '='
              | NOT_EQUAL
              | '<'
              | LESS_EQUAL
              | '>'
| GREATER_EQUAL

expr : termo
| expr op_ad termo

termo : fator
| termo op_mul fator 

op_ad : '+'
             | '-'
| OR

op_mul : '*'
             | '/'
             | AND
             | MOD
| DIV

fator : value
             | func_call
| '(' cond ')' 

func_call : ID '(' args ')'

args : elems
|

elems : elems ',' value
| value
"""

def p_program(p: YaccProduction):
    """program : PROGRAM ID ';' functions variables_declaration code_block '.'
               | code_block '.'"""

def p_functions(p):
    """functions : functions function
                 | function
                 |"""
    pass

def p_function(p):
    """function : FUNCTION ID '(' parameters ')' ':' DATATYPE ';' variables_declaration code_block ';'"""
    pass

def p_parameters(p):
    """parameters : parameter_list
                  |"""
    pass

def p_parameter_list(p):
    """parameter_list : parameter_list ';' parameter
                      | parameter"""
    pass

def p_parameter(p):
    """parameter : ID ':' DATATYPE"""
    pass

def p_variables_declaration(p: YaccProduction):
    """variables_declaration : VAR variables_list
                             |"""

def p_variables_list(p: YaccProduction):
    """variables_list : same_type_variables
                      | variables_list same_type_variables"""

def p_same_type_variables(p: YaccProduction):
    """same_type_variables : ID id_list ':' DATATYPE ';'
                           | ID id_list ':' ARRAY '[' INT RANGE INT ']' OF DATATYPE ';' """

def p_id_list(p: YaccProduction):
    """id_list : id_list ',' ID
               |"""

def p_code_block(p: YaccProduction):
    "code_block : BEGIN algorithm END"

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

def p_assignment(p: YaccProduction):
    """assignment : ID ASSIGNMENT assignment_value"""

def p_assignment_value(p: YaccProduction):
    """assignment_value : value
                        | expr"""

def p_value(p: YaccProduction):
    """value : ID
             | INT
             | REAL
             | STRING
             | BOOL
             | ID '[' INT ']' 
             | ID '[' ID ']'"""

def p_if_else(p):
    """if_else : IF cond THEN algorithm ELSE algorithm
               | assignment
               | func_call"""

def p_if(p):
    """if : IF cond THEN algorithm"""

def p_loop(p: YaccProduction):
    """loop : for
            | while"""

def p_for(p: YaccProduction):
    """for : FOR for_cond DO code_block
           | FOR for_cond DO algorithm"""

def p_for_cond(p: YaccProduction):
    """for_cond : cond 
                | assignment TO ID
                | assignment TO INT
                | assignment DOWNTO ID
                | assignment DOWNTO INT"""

def p_while(p: YaccProduction):
    """while : WHILE while_cond DO code_block
             | WHILE while_cond DO algorithm"""

def p_while_cond(p: YaccProduction):
    """while_cond : cond"""

def p_cond(p: YaccProduction):
    """cond : expr
            | expr op_rel expr"""

def p_op_rel(p: YaccProduction):
    """op_rel : '='
              | NOT_EQUAL
              | '<'
              | LESS_EQUAL
              | '>'
              | GREATER_EQUAL"""

def p_expr(p: YaccProduction):
    """expr : termo
            | expr op_ad termo"""

def p_termo(p: YaccProduction):
    """termo : fator
             | termo op_mul fator """

def p_op_ad(p: YaccProduction):
    """op_ad : '+'
             | '-'
             | OR"""

def p_op_mul(p: YaccProduction):
    """op_mul : '*'
             | '/'
             | AND
             | MOD
             | DIV"""

def p_fator(p: YaccProduction):
    """fator : value
             | func_call
             | '(' cond ')' """

def p_func_call(p: YaccProduction):
    """func_call : ID '(' args ')'"""

def p_args(p: YaccProduction):
    """args : elems
            |"""

def p_elems(p: YaccProduction):
    """elems : elems ',' value
             | value"""

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
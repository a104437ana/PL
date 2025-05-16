import sys
import ply.yacc as yacc
from ply.yacc import YaccProduction
from pascal_analex import tokens, literals
from pascal_exemplos import *

precedence = (
    ('nonassoc', 'IFX'),
    ('nonassoc', 'ELSE'),
)

def p_program(p: YaccProduction):
    """program : PROGRAM ID ';' declarations code_block '.'
               | declarations code_block '.'"""

def p_declarations(p):
    """declarations : declaration_list
                    |"""

def p_declaration_list(p):
    """declaration_list : declaration declaration_list
                        | declaration"""

def p_declaration(p):
    """declaration : variables_declaration
                   | function
                   | procedure"""

def p_variables_declaration(p: YaccProduction):
    """variables_declaration : VAR variables_list"""

def p_variables_list(p: YaccProduction):
    """variables_list : same_type_variables
                      | variables_list same_type_variables"""

def p_same_type_variables(p: YaccProduction):
    """same_type_variables : id_list ':' DATATYPE ';'
                           | id_list ':' ARRAY '[' INT RANGE INT ']' OF DATATYPE ';' """

def p_id_list(p: YaccProduction):
    """id_list : id_list ',' ID
               | ID"""

def p_var_or_not(p):
    """var_or_not : variables_declaration
                  |"""

def p_function(p):
    """function : FUNCTION ID '(' parameters ')' ':' DATATYPE ';' var_or_not code_block ';'"""

def p_procedure(p):
    """procedure : PROCEDURE ID '(' parameters ')' ';' var_or_not code_block ';'"""

def p_parameters(p):
    """parameters : parameter_list
                  |"""

def p_parameter_list(p):
    """parameter_list : parameter_list ';' parameter
                      | parameter"""

def p_parameter(p):
    """
    parameter : VAR_opt id_list ':' DATATYPE
    """

def p_VAR_opt(p):
    """
    VAR_opt : VAR
            |
    """

def p_code_block(p: YaccProduction):
    """code_block : BEGIN algorithm END"""

def p_algorithm(p):
    """algorithm : algorithm ';' statement
                 | statement"""

def p_statement(p):
    """statement : assignment
                 | func_call
                 | loop
                 | code_block
                 | if
                 | else
                 |"""

def p_if(p):
    """if : IF cond THEN statement %prec IFX"""

def p_else(p):
    """else : IF cond THEN statement ELSE statement"""

def p_assignment(p: YaccProduction):
    """assignment : ID ASSIGNMENT assignment_value"""

def p_assignment_value(p: YaccProduction):
    """assignment_value : expr"""

def p_value(p: YaccProduction):
    """value : ID
             | INT
             | REAL
             | STRING
             | BOOL
             | ID '[' INT ']' 
             | ID '[' ID ']'
             | func_call"""

def p_loop(p: YaccProduction):
    """loop : for
            | while"""

def p_for(p: YaccProduction):
    """for : FOR for_cond DO statement"""

def p_for_cond(p: YaccProduction):
    """for_cond : assignment TO ID
                | assignment TO INT
                | assignment DOWNTO ID
                | assignment DOWNTO INT"""

def p_while(p: YaccProduction):
    """while : WHILE cond DO statement"""

def p_cond(p: YaccProduction):
    """cond : expr op_rel expr
            | bool"""

def p_op_rel(p: YaccProduction):
    """op_rel : '='
              | NOT_EQUAL
              | '<'
              | LESS_EQUAL
              | '>'
              | GREATER_EQUAL"""

def p_bool(p):
    """bool : btermo
            | bool op_or btermo"""

def p_btermo(p):
    """btermo : bfator
              | btermo op_and bfator"""

def p_bfator(p):
    """bfator : BOOL
              | ID
              | ID '[' INT ']' 
              | ID '[' ID ']'
              | func_call
              | '(' cond ')'
              | NOT bfator"""

def p_expr(p: YaccProduction):
    """expr : termo
            | expr op_ad termo"""

def p_termo(p: YaccProduction):
    """termo : fator
             | termo op_mul fator """

def p_op_ad(p: YaccProduction):
    """op_ad : '+'
             | '-'"""

def p_op_or(p):
    """op_or : OR"""

def p_op_mul(p: YaccProduction):
    """op_mul : '*'
             | '/'
             | MOD
             | DIV"""

def p_op_and(p):
    "op_and : AND"

def p_fator(p: YaccProduction):
    """fator : INT
             | REAL
             | ID
             | ID '[' INT ']' 
             | ID '[' ID ']'
             | func_call
             | '(' expr ')'"""

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

parser = yacc.yacc(debug=True, write_tables=True, outputdir='.')
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
import ply.lex as lex
from pascal_exemplos import *

reserved = {
    'and' : 'AND',
    'array' : 'ARRAY',
    'begin' : 'BEGIN',
    'case' : 'CASE',
    'const' : 'CONST',
    'div' : 'DIV',
    'do' : 'DO',
    'downto' : 'DOWNTO',
    'else' : 'ELSE',
    'end' : 'END',
    'file' : 'FILE',
    'for' : 'FOR',
    'foward' : 'FOWARD',
    'function' : 'FUNCTION',
    'goto' : 'GOTO',
    'if' : 'IF',
    'in' : 'IN',
    'label' : 'LABEL',
    'mod' : 'MOD',
    'nil' : 'NIL',
    'not' : 'NOT',
    'of' : 'OF',
    'or' : 'OR',
    'packed' : 'PACKED',
    'procedure' : 'PROCEDURE',
    'program' : 'PROGRAM',
    'record' : 'RECORD',
    'repeat' : 'REPEAT',
    'set' : 'SET',
    'then' : 'THEN',
    'to' : 'TO',
    'type' : 'TYPE',
    'until' : 'UNTIL',
    'var' : 'VAR',
    'while' : 'WHILE',
    'with' : 'WITH'
}

tokens = [
    'NOT_EQUAL',
    'LESS_EQUAL',
    'GREATER_EQUAL',
    'ASSIGNMENT',
    'RANGE',
    'LPA','ARP','LPP','PRP',
    'ID',
    'INT',
    'REAL',
    'STRING',
    'COMMENT'
    
] + list(reserved.values())

literals = [
    '+',
    '-',
    '*',
    '/',
    '=',
    '<',
    '>',
    '[',
    ']',
    '.',
    ',',
    ':',
    ';',
    '^',
    '(',
    ')']

t_NOT_EQUAL = r'<>'
t_LESS_EQUAL = r'<='
t_GREATER_EQUAL = r'>='
t_ASSIGNMENT = r":="
t_RANGE = r'\.\.'

t_LPA = r'\(\*'
t_ARP = r'\*\)'
t_LPP = r'\(\.'
t_PRP = r'\.\)'

def t_AND(t):
	r'[aA][nN][dD]\b'
	t.type = 'AND'
	return t

def t_ARRAY(t):
	r'[aA][rR][rR][aA][yY]\b'
	t.type = 'ARRAY'
	return t

def t_BEGIN(t):
	r'[bB][eE][gG][iI][nN]\b'
	t.type = 'BEGIN'
	return t

def t_CASE(t):
	r'[cC][aA][sS][eE]\b'
	t.type = 'CASE'
	return t

def t_CONST(t):
	r'[cC][oO][nN][sS][tT]\b'
	t.type = 'CONST'
	return t

def t_DIV(t):
	r'[dD][iI][vV]\b'
	t.type = 'DIV'
	return t

def t_DO(t):
	r'[dD][oO]\b'
	t.type = 'DO'
	return t

def t_DOWNTO(t):
	r'[dD][oO][wW][nN][tT][oO]\b'
	t.type = 'DOWNTO'
	return t

def t_ELSE(t):
	r'[eE][lL][sS][eE]\b'
	t.type = 'ELSE'
	return t

def t_END(t):
	r'[eE][nN][dD]\b'
	t.type = 'END'
	return t

def t_FILE(t):
	r'[fF][iI][lL][eE]\b'
	t.type = 'FILE'
	return t

def t_FOR(t):
	r'[fF][oO][rR]\b'
	t.type = 'FOR'
	return t

def t_FOWARD(t):
	r'[fF][oO][wW][aA][rR][dD]\b'
	t.type = 'FOWARD'
	return t

def t_FUNCTION(t):
	r'[fF][uU][nN][cC][tT][iI][oO][nN]\b'
	t.type = 'FUNCTION'
	return t

def t_GOTO(t):
	r'[gG][oO][tT][oO]\b'
	t.type = 'GOTO'
	return t

def t_IF(t):
	r'[iI][fF]\b'
	t.type = 'IF'
	return t

def t_IN(t):
	r'[iI][nN]\b'
	t.type = 'IN'
	return t

def t_LABEL(t):
	r'[lL][aA][bB][eE][lL]\b'
	t.type = 'LABEL'
	return t

def t_MOD(t):
	r'[mM][oO][dD]\b'
	t.type = 'MOD'
	return t

def t_NIL(t):
	r'[nN][iI][lL]\b'
	t.type = 'NIL'
	return t

def t_NOT(t):
	r'[nN][oO][tT]\b'
	t.type = 'NOT'
	return t

def t_OF(t):
	r'[oO][fF]\b'
	t.type = 'OF'
	return t

def t_OR(t):
	r'[oO][rR]\b'
	t.type = 'OR'
	return t

def t_PACKED(t):
	r'[pP][aA][cC][kK][eE][dD]\b'
	t.type = 'PACKED'
	return t

def t_PROCEDURE(t):
	r'[pP][rR][oO][cC][eE][dD][uU][rR][eE]\b'
	t.type = 'PROCEDURE'
	return t

def t_PROGRAM(t):
	r'[pP][rR][oO][gG][rR][aA][mM]\b'
	t.type = 'PROGRAM'
	return t

def t_RECORD(t):
	r'[rR][eE][cC][oO][rR][dD]\b'
	t.type = 'RECORD'
	return t

def t_REPEAT(t):
	r'[rR][eE][pP][eE][aA][tT]\b'
	t.type = 'REPEAT'
	return t

def t_SET(t):
	r'[sS][eE][tT]\b'
	t.type = 'SET'
	return t

def t_THEN(t):
	r'[tT][hH][eE][nN]\b'
	t.type = 'THEN'
	return t

def t_TO(t):
	r'[tT][oO]\b'
	t.type = 'TO'
	return t

def t_TYPE(t):
	r'[tT][yY][pP][eE]\b'
	t.type = 'TYPE'
	return t

def t_UNTIL(t):
	r'[uU][nN][tT][iI][lL]\b'
	t.type = 'UNTIL'
	return t

def t_VAR(t):
	r'[vV][aA][rR]\b'
	t.type = 'VAR'
	return t

def t_WHILE(t):
	r'[wW][hH][iI][lL][eE]\b'
	t.type = 'WHILE'
	return t

def t_WITH(t):
	r'[wW][iI][tT][hH]\b'
	t.type = 'WITH'
	return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'ID'
    return t

def t_REAL(t):
    r'[+-]?(\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+)'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'[+-]?\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\'[^\']+\'|\"[^\"]*\"'
    return t

t_ignore_COMMENT = r"\{.*\}|\(\*.*\*\)"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Illegal character ('%s',%s,%s)"% (t.value[0], t.lineno, t.lexpos))
    t.lexer.skip(1)

def analex(text):
    lexer = lex.lex()
    lexer.input(text)
    for tok in lexer:
        print(tok)

analex(exemplo7)
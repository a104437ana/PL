import ply.lex as lex

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
    'QUOTE',
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
t_RANGE = r'..'

t_LPA = r'(*'
t_ARP = r'*)'
t_LPP = r'(.'
t_PRP = r'.)'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t_lower = t.value.lower()
    t.type = reserved.get(t_lower,'ID')
    return t

def t_REAL(t):
    r'-?\d+\.\d+([eE][-+]?\d+)?'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

def t_QUOTE(t):
    r'\'[^\']+\'|\"[^\"]+\"'
    return t

t_COMMENT = r"\{[^}]+\}"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Illegal character ('%s',%s,%s)"% (t.value[0], t.lineno, t.lexpos))
    t.lexer.skip(1)

lexer = lex.lex()

texto = """Program Maior3;
var
num1, num2, num3, maior: Integer;
begin
{ Ler 3 números }
Write('Introduza o primeiro número: ');
ReadLn(num1);
Write('Introduza o segundo número: ');
ReadLn(num2);
Write('Introduza o terceiro número: ');
ReadLn(num3);
{ Calcular o maior }
if num1 > num2 then
if num1 > num3 then maior := num1
else maior := num3
else
if num2 > num3 then maior := num2
else maior := num3;
{ Escrever o resultado }
WriteLn('O maior é: ', maior)
end."""

lexer.input(texto)
for tok in lexer:
    print(tok)
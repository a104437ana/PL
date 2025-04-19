import ply.lex as lex

reserved = {
    'program' : 'PROGRAM',
    'var' : 'VAR',
    'begin' : 'BEGIN',
    'end' : 'END',
    'and' : 'AND',#lógica
    'or' : 'OR',
    'not' : 'NOT',
    'if' : 'IF',#controlo de fluxo
    'else' : 'ELSE',
    'then' : 'THEN',
    'while' : 'WHILE',
    'for' : 'FOR',
    'do' : 'DO'
}

tokens = [
    'ID',
    'QUOTE',
    'COMMENT',
    'ASSIGNMENT'
] + list(reserved.values())

literals = [';','.','(',')',',','>','<']

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_QUOTE(t):
    r'\'[^\']+\'|\"[^\"]+\"'
    return t

t_COMMENT = r"\{[^}]+\}"

t_ASSIGNMENT = r":="

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Illegal character ('%s',%s,%s)"% (t.value[0], t.lineno, t.lexpos))
    t.lexer.skip(1)

lexer = lex.lex()

texto = """program Maior3;
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
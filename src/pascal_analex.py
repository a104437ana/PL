import ply.lex as lex

reserved = {
    'program' : 'PROGRAM',
    'var' : 'VAR',
    'begin' : 'BEGIN',
    'end' : 'END',
    'and' : 'AND',
}

tokens = [
    'ID'
] + list(reserved.values())

literals = [';','.']

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

texto = """programHelloWorld;
begin
end."""

lexer.input(texto)
for tok in lexer:
    print(tok)
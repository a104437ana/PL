import ply.lex as lex

tokens = (
)

t_ignore = " \t"

def t_newline(t):
    r"\n"
    t.lexer.lineo += len(t.value)

def t_error(t):
    print(f'Símbolo inválido na linha {t.lineo}: {t.value}')
    t.lexer.skip(1)
    pass

lexer = lex.lex()

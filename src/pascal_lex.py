import ply.lex as lex

tokens = [
            "PROGRAM", "SEMICOLON", "VAR", "BEGIN", "END", "PERIOD", # início e fim de estados
            "ID", "OPAREN", "CPAREN", "QUOTE", "COMMA", "COLON", "DATATYPE", "COMMENT", # geral
            "IF", "ELSE", "THEN", "WHILE", "FOR", "DO", # controlo de fluxo
            "TO", "DOWNTO", "ASSIGNMENT", # variáveis
            "ARRAY", "OBRACKET", "CBRACKET", "OF", "TWOPERIODS", # arrays
            "INT", "REAL", "PLUS", "MINUS", "TIMES", "DIV", "DIVISION", "MOD", # cálculos
            "EQUAL", "NOTEQUAL", "LESSERTHAN", "GREATERTHAN", "LESSEREQUALS", "GREATEREQUALS", # comparação
            "BOOL", "AND", "OR", "NOT", # lógica
            "IGNORE"
          ]
states = [("PROG", "exclusive"), ("VARS", "exclusive"), ("CODE", "exclusive")]

def t_PROGRAM(t):
    r'program'
    t.lexer.begin("PROG")
    return t

def t_PROG_SEMICOLON(t):
    r';'
    t.lexer.begin("INITIAL")
    return t

def t_VAR(t):
    r'var'
    t.lexer.begin("VARS")
    return t

def t_VARS_BEGIN(t):
    r'begin'
    t.lexer.begin("CODE")
    return t

def t_BEGIN(t):
    r'begin'
    t.lexer.begin("CODE")
    return t

def t_CODE_END(t):
    r'end'
    t.lexer.begin("INITIAL")
    return t

t_PERIOD = r'\.'

def t_PROG_ID(t):
    r'[\w|$]+'
    return t

def t_VARS_DATATYPE(t):
    r"integer|real|character|boolean|string"
    return t
def t_VARS_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t
def t_VARS_ID(t):
    r'[\w|$]+'
    return t
t_VARS_COMMA= r","
t_VARS_COLON = r":"
t_VARS_ARRAY = r"array"
t_VARS_OBRACKET = r"\["
t_VARS_CBRACKET = r"\]"
t_VARS_TWOPERIODS = r"\.\."
t_VARS_OF = r"of"
t_VARS_SEMICOLON = r";"

t_CODE_OPAREN = r"\("
t_CODE_CPAREN = r"\)"
t_CODE_SEMICOLON = r";"
t_CODE_COMMA = r","
t_CODE_IF = r"if"
t_CODE_ELSE = r"else"
t_CODE_THEN = r"then"
t_CODE_WHILE = r"while"
t_CODE_FOR = r"for"
t_CODE_DO = r"do"
t_CODE_TO = r"to"
t_CODE_DOWNTO = r"downto"
t_CODE_ASSIGNMENT = r":="
t_CODE_PLUS = r"\+"
t_CODE_MINUS = r"-"
t_CODE_TIMES = r"\*"
t_CODE_DIV = r"div"
t_CODE_DIVISION = r"\/"
t_CODE_MOD = r"mod"
t_CODE_EQUAL = r"="
t_CODE_NOTEQUAL = r"<>"
t_CODE_LESSERTHAN = r"<"
t_CODE_GREATERTHAN = r">"
t_CODE_LESSEREQUALS = r"<="
t_CODE_GREATEREQUALS = r">="
t_CODE_AND = r"and"
t_CODE_OR = r"or"
t_CODE_NOT = r"not"
def t_CODE_QUOTE(t):
    r'\'[^\']+\'|\"[^\"]+\"'
    return t
def t_CODE_REAL(t):
    r'-?\d+\.\d+'
    t.value = float(t.value)
    return t
def t_CODE_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t
def t_CODE_BOOL(t):
    r'true|false'
    return t
def t_CODE_ID(t):
    r'[\w|$]+'
    return t

t_ANY_COMMENT = r"\{[^}]+\}"

t_ANY_ignore = r"   "

def t_ANY_newline(t):
    r"\n"
    t.lexer.lineno += len(t.value)

def t_ANY_error(t):
    print(f'Símbolo inválido na linha {t.lineno}: {t.value}')
    t.lexer.skip(1)
    pass

lexer = lex.lex()

texto = """program HelloWorld;
begin
writeln('Ola, Mundo!');
end."""

texto2 = """program Maior3;
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

texto3 = """programHelloWorld;
begin
writeln('Ola, Mundo!');
end."""

lexer.input(texto2)
for tok in lexer:
    print(tok)
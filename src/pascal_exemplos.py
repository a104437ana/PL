exemplo1 = '''
program HelloWorld;
begin
  writeln('Ola, Mundo!');
end.
'''

exemplo2 = '''
program Maior3;
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
end.
'''

exemplo3 = '''
program Fatorial;
var
n, i, fat: integer;
begin
writeln('Introduza um número inteiro positivo:');
readln(n);
fat := 1;
for i := 1 to n do
fat := fat * i;
writeln('Fatorial de ', n, ': ', fat);
end.
'''

exemplo4 = '''
program NumeroPrimo;
var
num, i: integer;
primo: boolean;
begin
writeln('Introduza um número inteiro positivo:');
readln(num);
primo := true;
i := 2;
while (i <= (num div 2)) and primo do
begin
if (num mod i) = 0 then
primo := false;
i := i + 1;
end;
if primo then
writeln(num, ' é um número primo')
else
writeln(num, ' não é um número primo')
end.
'''

exemplo5 = '''
program SomaArray;
var
numeros: array[1..5] of integer;
i, soma: integer;
begin
soma := 0;
writeln('Introduza 5 números inteiros:');
for i := 1 to 5 do
begin
readln(numeros[i]);
soma := soma + numeros[i];
end;
writeln('A soma dos números é: ', soma);
end.
'''

exemplo6 = '''
program BinarioParaInteiro;
var
bin: string;
i, valor, potencia: integer;
begin
writeln('Introduza uma string binária:');
readln(bin);
valor := 0;
potencia := 1;
for i := length(bin) downto 1 do
begin
if bin[i] = '1' then
valor := valor + potencia;
potencia := potencia * 2;
end;
writeln('O valor inteiro correspondente é: ', valor);
end.
'''

exemplo7 = '''
program BinarioParaInteiro;
function BinToInt(bin: string): integer;
var
i, valor, potencia: integer;
begin
valor := 0;
potencia := 1;
for i := length(bin) downto 1 do
begin
if bin[i] = '1' then
valor := valor + potencia;
potencia := potencia * 2;
end;
BinToInt := valor;
end;
var
bin: string;
valor: integer;
begin
writeln('Introduza uma string binária:');
readln(bin);
valor := BinToInt(bin);
writeln('O valor inteiro correspondente é: ', valor);
end.
'''

exemplos = {
    "1": exemplo1,
    "2": exemplo2,
    "3": exemplo3,
    "4": exemplo4,
    "5": exemplo5,
    "6": exemplo6,
    "7": exemplo7,
}

vm1 ='''
START

PUSHS "Ola, Mundo!"
WRITES
WRITELN

STOP'''

vm2 ='''
PUSHI 0 // variavel num1
PUSHI 0 // variavel num2
PUSHI 0 // variavel num3
PUSHI 0 // variavel maior

START


PUSHS "Introduza o primeiro número: "
WRITES

READ
ATOI
STOREG 0

PUSHS "Introduza o segundo número: "
WRITES

READ
ATOI
STOREG 1

PUSHS "Introduza o terceiro número: "
WRITES

READ
ATOI
STOREG 2

PUSHG 0
PUSHG 1
SUP
JZ ELSE2 // if
PUSHG 0
PUSHG 2
SUP
JZ ELSE0 // if
PUSHG 0
STOREG 3
JUMP ENDIF0
ELSE0: // else
PUSHG 2
STOREG 3
JUMP ENDIF0
ENDIF0:
JUMP ENDIF2
ELSE2: // else
PUSHG 1
PUSHG 2
SUP
JZ ELSE1 // if
PUSHG 1
STOREG 3
JUMP ENDIF1
ELSE1: // else
PUSHG 2
STOREG 3
JUMP ENDIF1
ENDIF1:
JUMP ENDIF2
ENDIF2:

PUSHS "O maior é: "
WRITES
PUSHG 3
WRITEI
WRITELN

STOP'''

vm3 = '''
PUSHI 0 // variavel n
PUSHI 0 // variavel i
PUSHI 0 // variavel fat

START


PUSHS "Introduza um número inteiro positivo:"
WRITES
WRITELN

READ
ATOI
STOREG 4

PUSHI 1
STOREG 6

PUSHI 1
STOREG 5
LOOP0:
PUSHG 5
PUSHG 4
INFEQ
JZ ENDLOOP0
PUSHG 6
PUSHG 5
MUL
STOREG 6
PUSHG 5
PUSHI 1
ADD
STOREG 5
JUMP LOOP0
ENDLOOP0:

PUSHS "Fatorial de "
WRITES
PUSHG 4
WRITEI
PUSHS ": "
WRITES
PUSHG 6
WRITEI
WRITELN

STOP'''

vm4 ='''
PUSHI 0 // variavel num
PUSHI 0 // variavel i
PUSHI 0 // variavel primo

START


PUSHS "Introduza um número inteiro positivo:"
WRITES
WRITELN

READ
ATOI
STOREG 7

PUSHI 1
STOREG 9

PUSHI 2
STOREG 8

LOOP1:
PUSHG 8
PUSHG 7
PUSHI 2
DIV
INFEQ
PUSHG 9
AND
JZ ENDLOOP1
PUSHG 7
PUSHG 8
MOD
PUSHI 0
EQUAL
JZ ENDIF3 // if
PUSHI 0
STOREG 9
JUMP ENDIF3
ENDIF3:

PUSHG 8
PUSHI 1
ADD
STOREG 8

JUMP LOOP1
ENDLOOP1:

PUSHG 9
JZ ELSE4 // if
PUSHG 7
WRITEI
PUSHS " é um número primo"
WRITES
WRITELN
JUMP ENDIF4
ELSE4: // else
PUSHG 7
WRITEI
PUSHS " não é um número primo"
WRITES
WRITELN
JUMP ENDIF4
ENDIF4:

STOP'''

vm5='''
PUSHI 0 // variavel numeros
PUSHI 0 // variavel i
PUSHI 0 // variavel soma

START

// heap variavel numeros
PUSHI 5
ALLOCN
STOREG 10

PUSHI 0
STOREG 12

PUSHS "Introduza 5 números inteiros:"
WRITES
WRITELN

PUSHI 1
STOREG 11
LOOP2:
PUSHG 11
PUSHI 5
INFEQ
JZ ENDLOOP2
PUSHG 10
PUSHG 11
PUSHI 1
SUB
READ
ATOI
STOREN

PUSHG 12
PUSHG 10
PUSHG 11
PUSHI 1
SUB
LOADN
ADD
STOREG 12

PUSHG 11
PUSHI 1
ADD
STOREG 11
JUMP LOOP2
ENDLOOP2:

PUSHS "A soma dos números é: "
WRITES
PUSHG 12
WRITEI
WRITELN

STOP'''

vm6 = '''
PUSHI 0 // variavel bin
PUSHI 0 // variavel i
PUSHI 0 // variavel valor
PUSHI 0 // variavel potencia

START


PUSHS "Introduza uma string binária:"
WRITES
WRITELN

READ
STOREG 13

PUSHI 0
STOREG 15

PUSHI 1
STOREG 16

PUSHG 13
STRLEN
STOREG 14
LOOP3:
PUSHG 14
PUSHI 1
SUPEQ
JZ ENDLOOP3
PUSHG 13
PUSHG 14
PUSHI 1
SUB
CHARAT
PUSHS "1"
CHRCODE
EQUAL
JZ ENDIF5 // if
PUSHG 15
PUSHG 16
ADD
STOREG 15
JUMP ENDIF5
ENDIF5:

PUSHG 16
PUSHI 2
MUL
STOREG 16

PUSHG 14
PUSHI 1
SUB
STOREG 14
JUMP LOOP3
ENDLOOP3:

PUSHS "O valor inteiro correspondente é: "
WRITES
PUSHG 15
WRITEI
WRITELN

STOP'''

vm7=''''''

vm = {
  "1": vm1,
  "2": vm2,
  "3": vm3,
  "4": vm4,
  "5": vm5,
  "6": vm6,
  "7": vm7,
}
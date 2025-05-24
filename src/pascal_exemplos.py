exemplo0 = '''
program TesteCompleto;

var
  i, n, soma: integer;

procedure ImprimeLinha();
begin
  writeln('------------------------');
end;

function Fatorial(x: integer): integer;
var
  resultado, j: integer;
begin
  resultado := 1;
  for j := 1 to x do
    resultado := resultado * j;
  Fatorial := resultado;
end;

begin
  writeln('Programa Teste Completo Pascal');
  ImprimeLinha();

  writeln('Digite um número inteiro positivo:');
  readln(n);

  if n < 0 then
  begin
    writeln('Número inválido!');
  end
  else
  begin
    soma := 0;
    i := 0;
    while i <= n do
    begin
      soma := soma + i;
      i := i + 1;
    end;

    writeln('Soma dos números de 0 até ', n, ' é: ', soma);

    writeln('Fatorial de ', n, ' é: ', Fatorial(n));

    writeln('Contagem regressiva de 10 até 1:');
    for i := 10 downto 1 do
      writeln(i);

  end;

  ImprimeLinha();
  writeln('Fim do programa.');
end.
'''

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

exemplo8 = '''
program TesteGramatica;

var
  x, y, z: integer;

procedure MostrarValor(v: integer);
begin
  writeln('Valor: ', v);
end;

function Somar(a, b: integer): integer;
begin
  Somar := a + b;
end;

begin
  x := 10;
  y := 20;

  if x < y then
    MostrarValor(x)
  else
    MostrarValor(y);

  z := 0;
  for x := 1 to 5 do
  begin
    z := Somar(z, x);
    writeln('z agora é: ', z);
  end;

  while z > 0 do
  begin
    writeln('Decrementando z: ', z);
    z := z - 1;
  end;

end.

'''

exemplo9 = '''
program LoopTamanhoMenosUm;

var
  i, tamanho: integer;
  testeBooleano: boolean;

begin
  tamanho := 5 + 4;  { Você pode mudar esse valor }

  testeBooleano := "oi" <> "oii";  { Atribuição de booleano para teste sintático }

  for i := 0 to tamanho - 1 do
    writeln('Valor de i: ', i);

  for i := 0 to (tamanho - 1) do
    writeln('Valor de i: ', i);

  for i := 0 to tamanho <> 0 do
    writeln(i + 1);
end.
'''

exemplos = {
    "0": exemplo0,
    "1": exemplo1,
    "2": exemplo2,
    "3": exemplo3,
    "4": exemplo4,
    "5": exemplo5,
    "6": exemplo6,
    "7": exemplo7,
    "8": exemplo8,
    "9": exemplo9,
}
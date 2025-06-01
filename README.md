# Projeto PL - Construção de um Compilador para Pascal Standard
## Descrição
Este repositório foi criado para a Unidade Curricular de **Processamento de Linguagens** (**PL**) e contém o projeto que desenvolvemos ao longo do segundo semestre do ano letivo de 2024/2025, no âmbito desta disciplina.

O objetivo deste projeto é desenvolver um compilador para a linguagem Pascal standard.

O compilador deverá ser capaz de analisar, interpretar e traduzir código Pascal para um formato
intermediário e deste para código máquina ou diretamente para código máquina, neste caso para a [VM
disponibilizada aos alunos](https://ewvm.epl.di.uminho.pt/).

Para mais detalhes sobre os requisitos e objetivos do projeto, consulte o [enunciado](Enunciado.pdf).

Para mais detalhes sobre a implementação deste projeto, consulte o [relatório técnico](Relatório.pdf).

O código desenvolvido pode ser encontrado na pasta [src](src).

Os exemplos de código Pascal dados no enunciado estão na pasta [examples](examples).

O código máquina correspondente aos exemplos dados está na pasta [out](out).

O código para construir o relatório está na pasta [report](report).

## Autores
### Grupo 12 - Equipa Bugbusters 🪲🚫
- A104437 - Ana Sá Oliveira
- A104263 - Inês Silva Marques
- A76350 - José Rafael de Oliveira Vilas Boas

![BUGBUSTERS](report/cover/Bugbusters.png)

## Utilização - Exemplos
### Entrar na pasta src
```
cd src
```

### Compilador

...

### Testes
Para correr o teste geral com os 7 exemplos do enunciado fazemos:
```
python3 pascal_test.py
```

## Lexer

Se quisermos testar o lexer com o input do terminal:
```
python3 pascal_analex.py
```

Se quisermos testar o lexer com um ficheiro:
```
python3 pascal_analex.py < ../examples/exemplo1.pas
```

Se quisermos testar com um exemplo concreto do enunciado:
```
python3 pascal_analex.py 1
```

## Parser

Se quisermos testar o parser com o input do terminal:
```
python3 pascal_anasin.py
```

Se quisermos testar o parser com um ficheiro:
```
python3 pascal_anasin.py < ../examples/exemplo1.pas
```

Se quisermos testar com um exemplo concreto do enunciado:
```
python3 pascal_anasin.py 1
```

## Analisador Semântico

Se quisermos testar a análise semântica com o input do terminal:
```
python3 pascal_anasem.py
```

Se quisermos testar com um ficheiro:
```
python3 pascal_anasem.py < ../examples/exemplo1.pas
```

Se quisermos testar com um exemplo concreto do enunciado:
```
python3 pascal_anasem.py 1
```
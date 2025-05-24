# variáveis globais da ast
global_vars = {}        # dicionário ID -> posição no global pointer
global_vars_type = {}   # dicionário ID -> tipo da variável
known_functions = {}
last_global_pointer = 0 # última posição do global pointer

# metodos globais da ast


class Program:
    def __init__(self, id, declarations, code):
        self.id = id                        # ID do programa
        self.declarations = declarations    # lista de classes Declaration
        self.code = code                    # classe CodeBlock
    
    def generateVmCode(self):
        code = ""
        for decl in self.declarations:
            if isinstance(decl, Variables):
                code += f"\n{decl.generateVmCodePush()}"
            elif isinstance(decl, Function):
                code += f"\n" # ver como fazer
            elif isinstance(decl, Procedure):
                code += f"\n" # ver como fazer
        code += f"\nSTART\n"
        code += f"{self.code.generateVmCode()}"
        code += f"STOP"
        return code

    def __str__(self):
        prog = f"program {self.id};\n"
        for decl in self.declarations:
            prog += f"\n{str(decl)}"
        prog += f"\n{str(self.code)}."
        return prog

    __repr__ = __str__

class Declaration:
    pass

class Variable():
    def __init__(self, id, type, is_array=False, array_size=0, array_init=0, array_type=None, value=None):
        self.id = id                    # ID da variável
        self.type = type                # tipo da variável
        self.is_array = is_array        # se é um array
        self.array_size = array_size    # tamanho do array
        self.array_init = array_init    # primeiro índice do array
        self.array_type = array_type    # tipo de dados do array
        self.value = value              # valor

    def generateVmCode(self):
        return ""

    def generateVmCodePush(self):
        code = ""
        if self.type == "string":
            code += f'pushs ""'
        elif self.type == "character":
            code += f'pushs ""'
        elif self.type == "integer":
            code += f"pushi 0"
        elif self.type == "real":
            code += f"pusf 0.0"
        elif self.type == "boolean":
            code += f"pushi 0"
        return code

    def __str__(self):
        var = f"{self.id}: {self.type}"
        if self.is_array:
            var += f"[{self.array_init}..{self.array_init+self.array_size-1}] of {self.array_type};"
        return var

    __repr__ = __str__

class Variables(Declaration):
    def __init__(self, variables=None):
        self.variables = variables if variables is not None else {} # dicionário ID -> classe Variable
    
    def add(self, variable: Variable):
        if isinstance(variable, Variable):
            self.variables[variable.id] = variable
        elif isinstance(variable, Variables):
            for id, var in variable.variables.items():
                if var.id not in self.variables:
                    self.variables[var.id] = var
                else:
                    raise Exception(f"Variable {var.id} already defined")

    def items(self):
        return self.variables.items()

    def generateVmCode(self):
        return ""

    def generateVmCodePush(self):
        global last_global_pointer, global_vars, global_vars_type
        code = ""
        for id, var in self.variables.items():
            code += var.generateVmCodePush() + f" // variavel {var.id}\n"
            global_vars[var.id] = last_global_pointer
            global_vars_type[var.id] = var.type
            last_global_pointer += 1
        return code

    def __str__(self):
        if len(self.variables.items()) > 0:
            vars = f"var\n"
        else:
            return ""
        for id, var in self.variables.items():
            vars += "\t" + str(var) + "\n"
        return vars

    def strParams(self):
        params = ""
        for i, (id, param) in enumerate(self.variables.items()):
            if i != 0:
                params += " "
            params += str(param)
            if i != len(self.variables)-1:
                params += ";"
        return params

    __repr__ = __str__

class Function(Declaration):
    def __init__(self, id, parameters, return_type, vars, algorithm):
        self.id = id                        # ID da função
        self.parameters = parameters        # classe Variables com as variáveis dos parametros
        self.return_type = return_type      # tipo de retorno
        self.vars = vars                    # classe Variables com as variáveis da função
        self.algorithm = algorithm          # classe CodeBlock

    def generateVmCode(self):
        return ""

    def __str__(self):
        func = f"function {self.id}({self.parameters.strParams()}): {self.return_type};\n"
        func += f"{str(self.vars)}\n"
        func += f"{str(self.algorithm)};\n"
        return func

    __repr__ = __str__

class Procedure(Declaration):
    def __init__(self, id, parameters, vars, algorithm):
        self.id = id                    # ID do procedimento
        self.parameters = parameters    # classe Variables com as variáveis dos parametros
        self.vars = vars                # classe Variables com as variáveis da função
        self.algorithm = algorithm      # classe CodeBlock

    def generateVmCode(self):
        return ""

    def __str__(self):
        proc = f"procedure {self.id}({self.parameters.strParams()});\n"
        proc += f"{str(self.vars)}\n"
        proc += f"{str(self.algorithm)};\n"
        return proc

    __repr__ = __str__

class Algorithm:
    def __init__(self, statements=None):
        self.statements = statements if statements is not None else []  # lista de classes Statement

    def add(self, statement):
        if statement is not None:
            self.statements.append(statement)

    def generateVmCode(self):
        code = ""
        for statement in self.statements:
            code += f"{statement.generateVmCode()}\n"
        return code

    def __str__(self):
        algorithm = ""
        for statement in self.statements:
            algorithm += f"{str(statement)}\n"
        return algorithm

    __repr__ = __str__

class Statement:
    pass

class Assignment(Statement):
    def __init__(self, id, expr):
        self.id = id        # ID da variável do assignment
        self.expr = expr    # classe Expression

    def generateVmCode(self):
        return ""

    def __str__(self):
        assign = f"{self.id} := {str(self.expr)};"
        return assign

    __repr__ = __str__

class Loop(Statement):
    def __init__(self, loop_type, cond, statement=None):
        self.loop_type = loop_type                                      # tipo do loop
        self.cond = cond                                                # classe Condition (falta)
        self.statement = statement if statement is not None else []     # lista de classes Statement

    def generateVmCode(self):
        return ""

    def __str__(self):
        loop = ""
        if self.loop_type == "for":
            loop += f"for {str(self.cond)} do\n"
            for statement in self.statement:
                loop += f"{str(statement)}\n"
        elif self.loop_type == "while":
            loop += f"while {str(self.cond)} do\n"
            for statement in self.statement:
                loop += f"{str(statement)}\n"
        return loop

    __repr__ = __str__

class If(Statement):
    def __init__(self, cond, true_statement, false_statement=None):
        self.cond = cond                            # classe Condition (falta)
        self.true_statement = true_statement        # classe Statement
        self.false_statement = false_statement      # classe Statement

    def generateVmCode(self):
        return ""

    def __str__(self):
        if_statement = f"if {str(self.cond)} then\n"
        for statement in self.true_statement:
            if_statement += f"{str(statement)}\n"
        if self.false_statement:
            if_statement += "else\n"
            for i, statement in enumerate(self.false_statement):
                if_statement += f"{str(statement)}"
                if i != len(self.false_statement)-1:
                    if_statement += "\n"
        if_statement += ";"
        return if_statement

    __repr__ = __str__

class CodeBlock(Statement):
    def __init__(self, algorithm):
        self.algorithm = algorithm      # classe Algorithm

    def generateVmCode(self):
        code = ""
        code += f"{self.algorithm.generateVmCode()}"
        return code

    def __str__(self):
        code = f"begin\n"
        code += f"{str(self.algorithm)}"
        code += f"end"
        return code

    __repr__ = __str__

class Expression:
    pass

class BinaryOp(Expression):
    def __init__(self, left, op, right):
        self.left = left        # classe Expression
        self.op = op            # string operador
        self.right = right      # classe Expression

    def generateVmCode(self):
        return ""

    def __str__(self):
        bin_op = f"{str(self.left)} {self.op} {str(self.right)}"
        return bin_op

    __repr__ = __str__

class UnaryOp(Expression):
    def __init__(self, op, expr):
        self.op = op        # string operador
        self.expr = expr    # classe Expression

    def generateVmCode(self):
        return ""

    def __str__(self):
        unary_op = f"{self.op} {str(self.expr)}"
        return unary_op

    __repr__ = __str__

class Value(Expression):
    def __init__(self, value, type):
        self.value = value      # valor
        self.type = type        # string do tipo do valor

    def generateVmCode(self):
        return ""

    def __str__(self):
        return str(self.value)

    __repr__ = __str__

class FunctionCall(Expression):
    def __init__(self, id, args):
        self.id = str(id).lower()   # ID da função
        self.args = args            # lista de argumentos da função classes Value ou FunctionCall

    def generateVmCode(self):
        code = ""
        if self.id == "writeln":
            for arg in self.args:
                arg = str(arg).replace("'", '"')
                code += f"pushs {arg}\n"
                code += "writes\n"
        elif self.id == "write":
            for arg in self.args:
                arg = str(arg).replace("'", '"')
                code += f"pushs {arg}\n"
                code += "writes\n"
        elif self.id == "readln":
            global global_vars, global_vars_type
            code += "read\n"
            var_pointer = global_vars[str(self.args[0])]
            var_type = global_vars_type[str(self.args[0])]
            if var_type == "integer":
                code += f"atoi\n"
            elif var_type == "real":
                code += f"atof 0.0"
            elif var_type == "boolean":
                code += f"atoi 0"   ################ ver como fazer para booleans
            code += f"storeg {var_pointer}"
        return code

    def __str__(self):
        func_call = f"{self.id}("
        for i, arg in enumerate(self.args):
            if i != 0:
                func_call += ", "
            func_call += f"{str(arg)}"
        func_call += ")"
        return func_call

    __repr__ = __str__

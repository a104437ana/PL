class Program:
    def __init__(self, id, declarations, code):
        self.id = id
        self.declarations = declarations
        self.code = code

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
        self.id = id
        self.type = type
        self.is_array = is_array
        self.array_size = array_size
        self.array_init = array_init
        self.array_type = array_type
        self.value = value

    def __str__(self):
        var = f"{self.id}: {self.type}"
        if self.is_array:
            var += f"[{self.array_init}..{self.array_init+self.array_size-1}] of {self.array_type};"
        return var

    __repr__ = __str__

class Variables(Declaration):
    def __init__(self, variables=None):
        self.variables = variables if variables is not None else {} # dict
    
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
        self.id = id
        self.parameters = parameters
        self.return_type = return_type
        self.vars = vars
        self.algorithm = algorithm

    def __str__(self):
        func = f"function {self.id}({self.parameters.strParams()}): {self.return_type};\n"
        func += f"{str(self.vars)}\n"
        func += f"{str(self.algorithm)};\n"
        return func

    __repr__ = __str__

class Procedure(Declaration):
    def __init__(self, id, parameters, vars, algorithm):
        self.id = id
        self.parameters = parameters
        self.vars = vars
        self.algorithm = algorithm

    def __str__(self):
        proc = f"procedure {self.id}({self.parameters.strParams()});\n"
        proc += f"{str(self.vars)}\n"
        proc += f"{str(self.algorithm)};\n"
        return proc

    __repr__ = __str__

class Algorithm:
    def __init__(self, statements=None):
        self.statements = statements if statements is not None else []

    def add(self, statement):
        if statement is not None:
            self.statements.append(statement)

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
        self.id = id
        self.expr = expr

    def __str__(self):
        assign = f"{self.id} := {str(self.expr)};"
        return assign

    __repr__ = __str__

class Loop(Statement):
    def __init__(self, loop_type, cond, statement=[]):
        self.loop_type = loop_type
        self.cond = cond
        self.statement = statement

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
        self.cond = cond
        self.true_statement = true_statement
        self.false_statement = false_statement

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
        self.algorithm = algorithm

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
        self.left = left
        self.op = op
        self.right = right

    def __str__(self):
        bin_op = f"{str(self.left)} {self.op} {str(self.right)}"
        return bin_op

    __repr__ = __str__

class UnaryOp(Expression):
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

    def __str__(self):
        unary_op = f"{self.op} {str(self.expr)}"
        return unary_op

    __repr__ = __str__

class Value(Expression):
    def __init__(self, value, type):
        self.value = value
        self.type = type

    def __str__(self):
        return str(self.value)

    __repr__ = __str__

class FunctionCall(Expression):
    def __init__(self, id, args):
        self.id = id
        self.args = args

    def __str__(self):
        func_call = f"{self.id}("
        for i, arg in enumerate(self.args):
            if i != 0:
                func_call += ", "
            func_call += f"{str(arg)}"
        func_call += ")"
        return func_call

    __repr__ = __str__

# Author: John Guastavino

import sys
import tpg

variables = {}
procedures = {}
stack = list()

# ------------- BEGINNING OF STACK -------------------
def addFrame(frameType=None):
    pushFrame(StackFrame(frameType))

def removeStackFrame():
    popFrame()

def findFrame(ftype):
    global top,stack

    global top, stack
    index = top
    frameType = None

    while(index > 0):

        index = index - 1
        frameType = stack[index].getType()
        if frameType == ftype:
            return stack[index]

    return None


def pushFrame(frame):
    global top, stack

    stack.append(frame)
    top = top + 1

def popFrame():
    global top,stack
    value = None

    if(top > 0):
        value = stack.pop(top - 1)
        top = top -1
    else:
        raise SemanticError("Empty Stack")

    return value

def peekFrame():
    global top, stack

    if(top > 0):
        return stack[top -1]
    else:
        return None

def procGet(name):
    return procedures.get(name.getName())

def procSet(name, value=None):
    procedures[name.getName()] = value

def findValue(key):
    global top, stack
    topOfStack = top
    value = None

    while(topOfStack > 0):

        topOfStack = topOfStack - 1
        value = stack[topOfStack].get(key)
        if value is not None: break

    return value

def setValue(key, value):
    global top
    found = False;
    index = top

    while(index > 0):

        index = index - 1
        val = stack[index].get(key)
        if val is not None:
            found = True
            break

    setValueInFrame(key, value, index if(found) else top - 1)


def setValueInFrame(key, value, frame):
    global stack, top

    if(frame):
        stack[frame].putValue(key, value)
    else:
        stack[top -1].putValue(key, value)

# ------------ END OF STACK -----------------------

class StackFrame(object):

    def __init__(self, frameType=None):
        self.local_symbols = {}
        self.type=frameType

    def getType(self):
        return self.type

    def get(self, key, default=None):
        return self.local_symbols.get(key, default)

    def putValue(self, key, value=None):
        self.local_symbols[key] = value

class SemanticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """
class SyntacticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """
    
# These are the nodes of our abstract syntax tree.
class Node(object):
    """
    A base class for nodes. Might come in handy in the future.
    """

    def evaluate(self):
        """
        Called on children of Node to evaluate that child.
        """
        raise Exception("Not implemented.")

class ExecutableNode(Node):

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        raise SemanticError("Not implemented")

    def location(self):
        raise SemanticError("Not implemented")

    def execute(self):
        raise SemanticError("Not implemented")

class Program(Node):
    def __init__(self):
        self.blocks = list()
        addFrame()

    def addBlock(self, block):
        self.blocks.append(block)

    def execute(self):

        for i in self.blocks:
            i.execute()

class Variable(Node):
    def __init__(self, name):
        self.name = name


    def evaluate(self):
        return self.name

class IntLiteral(Node):
    """
    A node representing integer literals.
    """

    def __init__(self, value):
        self.value = int(value)

    def evaluate(self):
        return self.value

class RealLiteral(Node):
    """
    A node representing real literals.
    """

    def __init__(self, value):
        self.value = float(value)

    def evaluate(self):
        return float(self.value)

class StringLiteral(Node):
    """
    A node representing string literals.
    """
    # add [1:-1] to truncate leading and tailing "
    def __init__(self, value):
        self.value = str(value)[1:-1]

    def evaluate(self):
        return self.value
    
class Multiply(Node):
    """
    A node representing multiplication.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left * right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return float(left * right)
        else:
            raise SemanticError


class Divide(Node):
    """
    A node representing division.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left / right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return float(left / right)
        else:
            raise SemanticError

class Add(Node):
    """
    A node representing additiion.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int):
            return int(left + right)
        elif (isinstance(left, int) or isinstance(left, float)) and (isinstance(right, int) or isinstance(right, float)):
            return float(left + right)
        elif isinstance(left, str) and isinstance(right, str):
            return str(left + right)
        else:
            raise SemanticError()

class Subtract(Node):
    """
    A node representing subtraction.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        if isinstance(left, int) and isinstance(right, int):
            return left - right
        elif (isinstance(left.evaluate(), float) or isinstance(left, IntLiteral)) and (isinstance(right.evaluate(), float) or isinstance(right, IntLiteral)):
            return float(left - right)
        else:
            raise SemanticError

class And(Node):
    """
    A node representing boolean AND operation.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()

        return int(left and right)

class Or(Node):
    """
    A node representing boolean OR.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()

        return int(left or right)

class Not(Node):
    def __init__(self, notValue):
        self.value = notValue

    def evaluate(self):
        if isinstance(self.value, Variable):
            val = variables[self.value.evaluate()]
        else:
            val = self.value.evaluate()

        return bool(not(val))

class GreaterThan(Node):
    """
    A node representing >.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left > right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left > right
        else:
            raise SemanticError

class LessThan(Node):
    """
    A node representing <.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left < right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left < right
        else:
            raise SemanticError

class EqualTo(Node):
    """
    A node representing ==.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left == right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left == right
        else:
            raise SemanticError

class NotEqual(Node):
    """
    A node representing !=.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left != right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left != right
        else:
            raise SemanticError

class GreaterEqual(Node):
    """
    A node representing >=.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left >= right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left >= right
        else:
            raise SemanticError

class LessEqual(Node):
    """
    A node representing <=.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        # Performing error checking
        if isinstance(left, int) and isinstance(right, int):
            return left <= right
        elif (isinstance(left, float) or isinstance(left, int)) and (isinstance(right, float) or isinstance(right, int)):
            return left <= right
        else:
            raise SemanticError

class Xor(Node):
    """
    A node representing xor.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()
        # returns 0 for false and 1 for true
        return left ^ right

class In(Node):
    """
    A node representing boolean in.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, list):
            raise SemanticError()
        if right == 0:
            raise SemanticError()
        # returns 0 for false and 1 for true
        return bool(int(left) in list(right))

class Floor(Node):
    """
    A node representing //.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()
        if right == 0:
            raise SemanticError()
        # returns 0 for false and 1 for true
        return int(left // right)

class Exp(Node):
    """
    A node representing **.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()
        if right == 0:
            raise SemanticError()
        # returns 0 for false and 1 for true
        return int(left ** right)

class Index(Node):
    def __init__(self, left, right):
        # The nodes representing the left and right sides of this operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # original assignment of left and right
        left = self.left.evaluate()
        right = self.right.evaluate()
        right = right[0]

         # error checking
        if  not (isinstance(left, list) or isinstance(left, str) ):
             raise SemanticError();
        if not (isinstance(right, int) or isinstance((variables[right]), int)):
             raise SemanticError();

        if isinstance(left, str):
            left = variables[left]

        if isinstance(right, str):
            right = variables[right]

        return left[right];

class Modulo(Node):
    """
    A node representing %.
    """

    def __init__(self, left, right):
        # The nodes representing the left and right sides of this
        # operation.
        self.left = left
        self.right = right

    def evaluate(self):
        # Checking for variables and evaluating left and right
        if isinstance(self.left, Variable):
            left = variables[self.left.evaluate()]
        else:
            left = self.left.evaluate()
        if isinstance(self.right, Variable):
            right = variables[self.right.evaluate()]
        else:
            right = self.right.evaluate()

        if not isinstance(left, int):
            raise SemanticError()
        if not isinstance(right, int):
            raise SemanticError()
        if right == 0:
            raise SemanticError()
        # returns 0 for false and 1 for true
        return left % right

class List(Node):
    """
    A node representing List function.
    """

    def __init__(self, value):
        self.value = value

    def evaluate(self):
        return self.value

class Block(Node):
    def __init__(self, body):
        self.body = body

    def execute(self):
        for s in self.body:
            s.execute()

class Print(Node):
    def __init__(self, expression):
        self.expression = expression

    def execute(self):
        if isinstance(self.expression, Variable):
            print(variables[self.expression.evaluate()])
        else:
            print(self.expression.evaluate())

class Assign(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def execute(self):
        rightValue = 0;
        if isinstance(self.right, Variable):
            rightValue = variables[self.right.evaluate()]
            variables[self.left.evaluate()] = variables[self.right.evaluate()]
        else:
            rightValue = self.right.evaluate()

        if isinstance(self.left, Index):
            variableName = self.left.left.evaluate()
            variableIndex = self.left.right.evaluate()[0]

            if isinstance(variableIndex, str):
                variableIndex = int(variables[variableIndex])

            variable = variables[variableName]
            variable[variableIndex] = rightValue
        else:
            variables[self.left.evaluate()] = rightValue

class If(Node):
    def __init__(self, expression, body):
        self.expression = expression
        self.body = body

    def execute(self):
        if (self.expression.evaluate()):
            self.body.execute()

class IfElse(Node):
    def __init__(self, expression, ifBody, elseBody):
        self.expression = expression
        self.ifBody = ifBody
        self.elseBody = elseBody

    def execute(self):
        if (self.expression.evaluate()):
            self.ifBody.execute()
        else:
            self.elseBody.execute()

class While(Node):
    def __init__(self, expression, body):
        self.expression = expression
        self.body = body

    def execute(self):
        while (self.expression.evaluate()):
            self.body.execute()

class ProcDef(ExecutableNode):
    def __init__(self):
        print("ProcDef: _init_")

    def execute(self):
        print("ProcDef: execute")

    def setVariablesBlock(self, variables, block):
        self.variables = variables.evaluateAsVar()
        self.block = block

    def getVariables(self):
        return self.variables

    def getBlock(self):
        return self.block

class ProcedureCall(ExecutableNode):

    def __init__(self, name, var):
        self.name = name
        self.boundVariables = var

    def bindVariables(self, values):
        self.boundVariables = values

    def execute(self):
        return self.evaluate()

    def evaluate(self):
        procProtocol = procGet(self.name)

        boundVariables = self.boundVariables.evaluate()
        variables = procProtocol.getVariables()

        if(len(boundVariables) != len(variables)):
            raise SemanticError("Given ", str(len(boundVariables))," variables, expected ",str(len(variables)))

        addFrame(self.name)
        for i,key in enumerate(variables):
            value = boundVariables[i]
            setValue(key, value)
            print("Added Value: ",repr(value)," to key: ",key)
        procProtocol.getBlock().execute()
        returnVal = findValue("return")
        removeStackFrame()

        if(returnVal):
            return returnVal
        else:
            raise SemanticError("Error getting return value from method")

class Return(ExecutableNode):
    def __init__(self, expression):
        self.expression = expression
        self.methodReturn = "return"

    def execute(self):
        value = self.expression.evaluate()

        tempStack = list()
        total = 0

        while(peekFrame().getType() is None):
            tempStack.append(popFrame())
            total = total + 1

        setValue(self.methodReturn, value)

        while(total > 0):
            pushFrame(tempStack.pop(total - 1))
            total = total - 1

# This is the TPG Parser that is responsible for turning our language into
# an abstract syntax tree.
class Parser(tpg.Parser):
    """
    token real '\d*\.\d+|\d+\.\d*' RealLiteral;
    token int '\d+' IntLiteral;
    token string '\\"[^\\"]*\\"' StringLiteral;
    token variable '[A-Za-z][A-Za-z0-9]*' Variable;
    separator space "\s";
    
    START/a -> body/a
    ;

    body/a -> ( _func_def/a
              | block/a
              | code/a
    );

    block/a -> '{'          $ a=[] $
               ( body/b     $ a.append(b) $
               )* '}'       $ a = Block(a) $
    ;

    code/a -> ( _if_else/a
              | _if/a
              | _while/a
              | lines/a
    );

    lines/a -> ( _assign/a
               | _print/a
               | _return/a
               | _function/a
               ) ';'
    ;

    _func_def/a -> variable/v params/n block/b                            $ a = ProcDef(v, n, b) $ ;
    _if_else/a ->  'if' '\(' expression/b '\)' body/a 'else' body/c       $ a = IfElse(b, a, c) $ ;
    _if/a ->       'if' '\(' expression/b '\)' body/a                     $ a = If(b, a) $ ;
    _while/a ->    'while' '\(' expression/b '\)' body/a                  $ a = While(b, a) $ ;
    _assign/a ->   expression/a "=(?!=)" expression/b                     $ a = Assign(a, b) $ ;
    _print/a ->    'print' expression/b ';'                               $ a = Print(b)$ ;
    _return/a ->   'return ' expression/a                                 $ a = Return(a) $ ;
    _function/a -> variable/v param_list/l                                $ a = ProcedureCall(v, l) $ ;

    expression/a ->   Or/a
                    | And/a
                    | Not/a
                    | Compare/a
                    | Xor/a
                    | In/a
                    | addsub/a
                    | Floor/a
                    | Mod/a
                    | muldiv/a
                    | Exp/a
                    | index/a
                    | parens/a
    ;

    Or/a -> And/a ( "or" And/b              $ a = Or(a, b) $
    )*
    ;

    And/a -> Not/a ( "and" Not/b            $ a = And(a, b) $
    )*
    ;

    Not/a -> Compare/a | "not" Compare/b    $ a = Not(b) $
    ;

    Compare/a -> Xor/a ( ">=" Xor/b         $ a = GreaterEqual(a, b) $
                       | ">"  Xor/b         $ a = GreaterThan(a, b) $
                       | "<>" Xor/b         $ a = NotEqual(a, b) $
                       | "==" Xor/b         $ a = EqualTo(a, b) $
                       | "<=" Xor/b         $ a = LessEqual(a, b) $
                       | "<"  Xor/b         $ a = LessThan(a, b) $
    )*
    ;

    Xor/a -> In/a ( "xor" In/b              $ a = Xor(a, b) $
    )*
    ;

    In/a -> addsub/a ( "in" addsub/b        $ a = In(a, b) $
    )*
    ;

    addsub/a -> Floor/a ( variable/a "\+"   muldiv/b $(a, b) $
                        | "\+" Floor/b      $ a = Add(a, b) $
                        | "\-" Floor/b      $ a = Subtract(a, b) $
    )*
    ;

    Floor/a -> Exp/a ( "//" Exp/b           $ a = Floor(a, b) $
    )*
    ;

    Exp/a -> Mod/a ( "\*\*" Mod/b           $ a = Exp(a, b) $
    )*
    ;

    Mod/a -> muldiv/a ( "\%" muldiv/b       $ a = Modulo(a, b) $
    )*
    ;

    muldiv/a -> index/a ( "\*" index/b      $ a = Multiply(a, b) $
                        | "/"  index/b      $ a = Divide(a, b) $
    )*
    ;

    index/a -> parens/a ( int/b $ a = Index(a, b) $
                        | string/b $ a = Index(a, b) $
                        | list/b $ a = Index(a, b) $
    )* ;

    parens/a -> "\(" expression/a "\)" | literal/a
    ;

    literal/a -> int/a
                |real/a
                |string/a
                |list/a
                |variable/a
    ;

    list/a -> "\["          $a = [] $
              (expression/b $ a.append(b.evaluate()) $) ?
              ("," expression/b $ a.append(b.evaluate()) $) *
              $a = List(a) $ "\]"
    ;

    params/a -> "\("                $ a = [] $
                ( variable/v        $ a.append(v) $ )?
                ( "," variable/v    $ a.append(v) $ )*
                "\)"
    ;

    param_list/a -> "\("                $ a = [] $
                    ( expression/e      $ a.append(e) $ )?
                    ( "," expression/e  $ a.append(e) $ )*
                    "\)"
    ;
    """

# Make an instance of the parser
parse = Parser()

# Open the file containing the input.
try:
    f = open(sys.argv[1], "r")
except(IndexError, IOError):
    f = open("input3.txt", "r")

# read the input file
program = f.read()

# close input file
f.close()

try:
    # Try to parse the program
    node = parse(program)

    # Execute the node
    node.execute()

    # If an exception is thrown, print the appropriate error.
except tpg.Error:
    print("SYNTAX ERROR")
    # Uncomment the next line to re-raise the syntax error,
    # displaying where it occurs. Comment it for submission.
    raise


        
except SemanticError:
    print("SEMANTIC ERROR")
    # Uncomment the next line to re-raise the semantic error,
    # displaying where it occurs. Comment it for submission.
    #raise

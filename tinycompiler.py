# 1. syntree.py
class LabelFactory: # this is a suffix to add to all function names
    counter = 0     # in particular, it is useful for function overloading
    @staticmethod   # it is also useful for different goto labels (loops, conditional statements etc) in assembly code
    def cur_label():
        return "uniqstr%d" % LabelFactory.counter
    def new_label():
        LabelFactory.counter += 1
        return "uniqstr%d" % LabelFactory.counter

class Type:
    VOID   = 0
    INT    = 1
    BOOL   = 2
    STRING = 3

class Function:
    def __init__(self, name, args, var, fun, body, deco):
        self.name = name                                                 # function name, string
        self.args = args                                                 # function arguments, list of tuples (name, type)
        self.var  = var                                                  # local variables, list of tuples (name, type)
        self.fun  = fun                                                  # nested functions, list of Function nodes
        self.body = body                                                 # function body, list of statement nodes (Print/Return/Assign/While/IfThenElse/FunCall)
        self.deco = deco | {'label' : name+'_'+LabelFactory.new_label()} # decoration dictionary to be filled by the parser (line number) and by the semantic analyzer (return type, scope id etc)

# statements
class Print:
    def __init__(self, expr, newline, deco):
        self.expr, self.newline, self.deco = expr, newline, deco

class Return:
    def __init__(self, expr, deco):
        self.expr, self.deco = expr, deco

class Assign:
    def __init__(self, name, expr, deco):
        self.name, self.expr, self.deco = name, expr, deco

class While:
    def __init__(self, expr, body, deco):
        self.expr, self.body, self.deco = expr, body, deco

class IfThenElse:
    def __init__(self, expr, ibody, ebody, deco):
        self.expr, self.ibody, self.ebody, self.deco = expr, ibody, ebody, deco

# expressions
class ArithOp:
    def __init__(self, op, left, right, deco):
        self.op, self.left, self.right, self.deco = op, left, right, deco | {'type' : Type.INT}

class LogicOp:
    def __init__(self, op, left, right, deco):
        self.op, self.left, self.right, self.deco = op, left, right, deco | {'type' : Type.BOOL}

class Integer:
    def __init__(self, value, deco):
        self.value, self.deco = value, deco | {'type' : Type.INT}

class Boolean:
    def __init__(self, value, deco):
        self.value, self.deco = value, deco | {'type' : Type.BOOL}

class String:
    def __init__(self, value, deco):
        self.value, self.deco = value, deco | {'type' : Type.STRING, 'label' : LabelFactory.new_label() }

class Var:
    def __init__(self, name, deco):
        self.name, self.deco = name, deco

class FunCall: # depending on the context, a function call can be a statement or an expression
    def __init__(self, name, args, deco):
        self.name, self.args, self.deco = name, args, deco



# 2. lexer.py
class Token:
    def __init__(self, t, v, l=None):
        self.type, self.value, self.lineno = t, v, (l or 0)

    def __repr__(self):
         return f'Token(type={self.type!r}, value={self.value!r}, lineno={self.lineno!r})'

class WendLexer:
    keywords    = {'true':'BOOLEAN','false':'BOOLEAN','print':'PRINT','println':'PRINT','int':'TYPE','bool':'TYPE','if':'IF','else':'ELSE','while':'WHILE','return':'RETURN'}
    double_char = {'==':'COMP', '<=':'COMP', '>=':'COMP', '!=':'COMP', '&&':'AND', '||':'OR'}
    single_char = {'=':'ASSIGN','<':'COMP', '>':'COMP', '!':'NOT', '+':'PLUS', '-':'MINUS', '/':'DIVIDE', '*':'TIMES', '%':'MOD','(':'LPAREN',')':'RPAREN', '{':'BEGIN', '}':'END', ';':'SEMICOLON', ',':'COMMA'}
    tokens      = {'ID', 'STRING', 'INTEGER'} | { v for k, v in keywords.items() | double_char.items() | single_char.items() }

    def tokenize(self, text):
        lineno, idx, state, accum = 0, 0, 0, ''
        while idx<len(text):
            sym1 = text[idx+0] if idx<len(text)-0 else ' '
            sym2 = text[idx+1] if idx<len(text)-1 else ' '
            if state==0: # start scanning a new token
                if sym1 == '/' and sym2 == '/':   # start a comment scan
                    state = 1
                elif sym1.isalpha() or sym1=='_': # start a word scan
                    state = 3
                    accum += sym1
                elif sym1.isdigit():              # start a number scan
                    state = 4
                    accum += sym1
                elif sym1 == '"':                 # start a string scan
                    state = 2
                elif sym1 + sym2 in self.double_char:  # emit two-character token
                    yield Token(self.double_char[sym1+sym2], sym1+sym2, lineno)
                    idx += 1
                elif sym1 in self.single_char:         # emit one-character token
                    yield Token(self.single_char[sym1], sym1, lineno)
                elif sym1 not in ['\r', '\t', ' ', '\n']: # ignore whitespace
                    raise Exception(f'Lexical error: illegal character \'{sym1}\' at line {lineno}')
            elif state==3:                                          # scanning a word, check next character
                if sym1.isalpha() or sym1=='_' or  sym1.isdigit():  # still word?
                    accum += sym1                                   # if yes, continue
                else:                                               # otherwise the scan stops, we have a word
                    if accum in self.keywords:                           # is the word reserved?
                        yield Token(self.keywords[accum], accum, lineno) # if yes, keyword
                    else:
                        yield Token('ID', accum, lineno)            # identifier otherwise
                    idx -= 1
                    state, accum = 0, '' # start new scan
            elif state==4:                                          # scanning a number
                if sym1.isdigit():                                  # is next character a digit?
                    accum += sym1                                   # if yes, continue
                else:
                    yield Token('INTEGER', accum, lineno)           # otherwise, emit number token
                    idx -= 1
                    state, accum = 0, '' # start new scan
            elif state==2:                                          # scanning a string, check next character
                if sym1 != '"' or accum and accum[-1]=='\\':        # if not quote mark (or if escaped quote mark),
                    accum += sym1                                   # continue the scan
                else:
                    yield Token('STRING', accum, lineno) # otherwise emit the token
                    state, accum = 0, '' # start new scan
            if sym1 == '\n':
                lineno += 1
                if state==1: # if comment, start new scan
                    state, accum = 0, ''
            idx += 1
        if state:
            raise Exception('Lexical error: unexpected EOF')

# 3. parser.py
# from lexer import WendLexer
# from syntree import *

class ParseState:
    def __init__(self, rule, dot, start, token = None, prev = None):
        self.rule  = rule  # index of the parse rule in the grammar
        self.dot   = dot   # index of next symbol in the rule (dot position)
        self.start = start # we saw this many tokens when we started the rule
        self.token = token # we saw this many tokens up to the current dot position   # these two members are not necessary for
        self.prev  = prev  # parent parse state pointer                               # the recogninzer, but are handy to retrieve a parse path

    def next_symbol(self):
        prod = WendParser.grammar[self.rule][1]
        return prod[self.dot] if self.dot<len(prod) else None

    def __eq__(self, other):
        return self.rule == other.rule and self.dot == other.dot and self.start == other.start # NB no self.token, no self.prev

class WendParser: # the grammar is a list of triplets [nonterminal, production rule, AST node constructor]
    grammar = [['fun',            ['fun_type', 'ID', 'LPAREN', 'param_list', 'RPAREN', 'BEGIN', 'var_list', 'fun_list', 'statement_list', 'END'],
                                                                                                      lambda p: Function(p[1].value, p[3], p[6], p[7], p[8], {'type':p[0], 'lineno':p[1].lineno})],
               ['var',            ['TYPE', 'ID'],                                                     lambda p: Var(p[1].value, {'type':Type.INT if p[0].value=='int' else Type.BOOL, 'lineno':p[0].lineno})],
               ['param_list',     ['var'],                                                            lambda p: p],
               ['param_list',     [],                                                                 lambda p: p],
               ['param_list',     ['param_list', 'COMMA', 'var'],                                     lambda p: p[0] + [ p[2] ]],
               ['fun_type',       ['TYPE'],                                                           lambda p: Type.INT if p[0].value=='int' else Type.BOOL],
               ['fun_type',       [],                                                                 lambda p: Type.VOID],
               ['var_list',       ['var_list', 'var', 'SEMICOLON'],                                   lambda p: p[0] + [ p[1] ]],
               ['var_list',       [],                                                                 lambda p: p],
               ['fun_list',       ['fun_list', 'fun'],                                                lambda p: p[0] + [ p[1] ]],
               ['fun_list',       [],                                                                 lambda p: p],
               ['statement_list', ['statement_list', 'statement'],                                    lambda p: p[0] + [ p[1] ]],
               ['statement_list', [],                                                                 lambda p: p],
               ['statement',      ['ID', 'LPAREN', 'arg_list', 'RPAREN', 'SEMICOLON'],                lambda p: FunCall(p[0].value, p[2], {'lineno':p[0].lineno})],
               ['statement',      ['ID', 'ASSIGN', 'expr', 'SEMICOLON'],                              lambda p: Assign(p[0].value, p[2], {'lineno':p[0].lineno})],
               ['statement',      ['RETURN', 'expr', 'SEMICOLON'],                                    lambda p: Return(p[1], {'lineno':p[0].lineno})],
               ['statement',      ['RETURN', 'SEMICOLON'],                                            lambda p: Return(None, {'lineno':p[0].lineno})],
               ['statement',      ['PRINT', 'expr', 'SEMICOLON'],                                     lambda p: Print(p[1], p[0].value=='println', {'lineno':p[0].lineno})],
               ['statement',      ['IF', 'expr', 'BEGIN', 'statement_list', 'END', 'else_statement'], lambda p: IfThenElse(p[1], p[3], p[5], {'lineno':p[0].lineno})],
               ['else_statement', ['ELSE', 'BEGIN', 'statement_list', 'END'],                         lambda p: p[2]],
               ['else_statement', [],                                                                 lambda p: p],
               ['statement',      ['WHILE', 'expr', 'BEGIN', 'statement_list', 'END'],                lambda p: While(p[1], p[3], {'lineno':p[0].lineno})],
               ['arg_list',       ['expr'],                                                           lambda p: p],
               ['arg_list',       ['arg_list', 'COMMA', 'expr'],                                      lambda p: p[0] + [ p[2] ]],
               ['arg_list',       [],                                                                 lambda p: p],
               ['expr',           ['conjunction'],                                                    lambda p: p[0]],
               ['expr',           ['expr', 'OR', 'conjunction'],                                      lambda p: LogicOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['expr',           ['STRING'],                                                         lambda p: String(p[0].value, {'lineno':p[0].lineno})],
               ['conjunction',    ['literal'],                                                        lambda p: p[0]],
               ['conjunction',    ['conjunction', 'AND', 'literal'],                                  lambda p: LogicOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['literal',        ['comparand'],                                                      lambda p: p[0]],
               ['literal',        ['NOT', 'comparand'],                                               lambda p: LogicOp('==', Boolean(False, {}), p[1], {'lineno':p[0].lineno})],
               ['comparand',      ['addend'],                                                         lambda p: p[0]],
               ['comparand',      ['addend', 'COMP', 'addend'],                                       lambda p: LogicOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['addend',         ['term'],                                                           lambda p: p[0]],
               ['addend',         ['addend', 'MINUS', 'term'],                                        lambda p: ArithOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['addend',         ['addend', 'PLUS', 'term'],                                         lambda p: ArithOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['term',           ['factor'],                                                         lambda p: p[0]],
               ['term',           ['term', 'MOD', 'factor'],                                          lambda p: ArithOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['term',           ['term', 'DIVIDE', 'factor'],                                       lambda p: ArithOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['term',           ['term', 'TIMES', 'factor'],                                        lambda p: ArithOp(p[1].value, p[0], p[2], {'lineno':p[1].lineno})],
               ['factor',         ['atom'],                                                           lambda p: p[0]],
               ['factor',         ['PLUS', 'atom'],                                                   lambda p: p[1]],
               ['factor',         ['MINUS', 'atom'],                                                  lambda p: ArithOp('-', Integer(0, {}), p[1], {'lineno':p[0].lineno})],
               ['atom',           ['BOOLEAN'],                                                        lambda p: Boolean(p[0].value=='true', {'lineno':p[0].lineno})],
               ['atom',           ['INTEGER'],                                                        lambda p: Integer(int(p[0].value), {'lineno':p[0].lineno})],
               ['atom',           ['ID', 'LPAREN', 'arg_list', 'RPAREN'],                             lambda p: FunCall(p[0].value, p[2], {'lineno':p[0].lineno})],
               ['atom',           ['ID'],                                                             lambda p: Var(p[0].value, {'lineno':p[0].lineno})],
               ['atom',           ['LPAREN', 'expr', 'RPAREN'],                                       lambda p: p[1]]]

    def recognize(self, tokens): # check the syntax
        charts, self.seen = [[ParseState(0,0,0)]], []

        def append(i, state):
            if len(charts)==i: charts.append([])
            if state not in charts[i]: charts[i].append(state)

        while not self.seen or self.seen[-1]:    # fetch tokens one by one until end of file
            self.seen.append(next(tokens, None)) # keep all the tokens we encounter
            pos = len(self.seen)-1
            i = 0
            while i < len(charts[pos]):          # iterate through all Earley items in current chart
                state  = charts[pos][i]
                symbol = state.next_symbol()     # next symbol in the production rule
                if symbol is None:               # if no symbol: completed state
                    for item in charts[state.start]:
                        if item.next_symbol() == self.grammar[state.rule][0]:
                            append(pos, ParseState(item.rule, item.dot+1, item.start, pos, state))
                elif symbol in WendLexer.tokens: # if next symbol is a terminal,
                    if self.seen[-1] and symbol == self.seen[-1].type:  # scan a token
                        append(pos+1, ParseState(state.rule, state.dot+1, state.start, pos+1, state))
                else:                            # if next symbol is nonterminal, emit a prediction state
                    for idx, (lhs, rhs, _) in enumerate(self.grammar):
                        if lhs == symbol:
                            append(pos, ParseState(idx, 0, pos, pos, state))
                i += 1
            if self.seen[-1] and len(charts)==pos+1:
                raise Exception(f'Syntax error at line {self.seen[-1].lineno}, token={self.seen[-1].type}')
        cur = [ state for state in charts[-1] if state == ParseState(0, len(self.grammar[0][1]), 0) ] # all completed states at the end of the parse
        if not cur: # no final production rule found
            raise Exception('Syntax error: unexpected EOF')
        return cur[0]

    def build_syntree(self, rule):         # recover a parse path and build the syntax tree
        production = []                    # the production sequence:
        while rule:                        # rewind through the charts
            if rule.next_symbol() is None: # keep completed rules only
                production.append(rule)
            rule = rule.prev

        stack, token = [], 0               # now apply production rules in order: build a stack from the input rules
        for rule in reversed(production):  # chomp and chew then according to the production rules, put one symbol back after each chomp
            stack += self.seen[token:rule.token]
            token = rule.token
            chomp = len(self.grammar[rule.rule][1])        # number of symbols in the production rule
            chew  = []
            if chomp>0:                                    # chomp those symbols from the stack
                chew = stack[-chomp:]
                del stack[-chomp:]
            stack.append(self.grammar[rule.rule][2](chew)) # put AST node back on the stack
        return stack[0] # normally we have only one symbol left on the stack

    def parse(self, tokens):
        return self.build_syntree( self.recognize(tokens) )


# 4. analyzer.py (including symtable.py)

class SymbolTable():
    def __init__(self):
        self.variables = [{}]     # stack of variable symbol tables
        self.functions = [{}]     # stack of function symbol tables
        self.ret_stack = [ None ] # stack of enclosing function symbols, useful for return statements
        self.scope_cnt = 0        # global scope counter for the display table allocation

    def add_fun(self, name, argtypes, deco):  # a function can be identified by its name and a list of argument types, e.g.
        signature = (name, *argtypes)         # fun foo(x:bool, y:int) : int {...} has ('foo',Type.BOOL,Type.INT) signature
        if signature in self.functions[-1]:
            raise Exception('Double declaration of the function %s %s' % (signature[0], signature[1:]))
        self.functions[-1][signature] = deco
        deco['scope'] = self.scope_cnt # id for the function block in the scope display table
        self.scope_cnt += 1

    def add_var(self, name, deco):
        if name in self.variables[-1]:
            raise Exception('Double declaration of the variable %s' % name)
        self.variables[-1][name] = deco
        deco['scope']  = self.ret_stack[-1]['scope']   # pointer to the display entry
        deco['offset'] = self.ret_stack[-1]['var_cnt'] # id of the variable in the corresponding stack frame
        self.ret_stack[-1]['var_cnt'] += 1

    def push_scope(self, deco):
        self.variables.append({})
        self.functions.append({})
        self.ret_stack.append(deco)
        deco['var_cnt'] = 0 # reset the per scope variable counter

    def pop_scope(self):
        self.variables.pop()
        self.functions.pop()
        self.ret_stack.pop()

    def find_var(self, name):
        for i in reversed(range(len(self.variables))):
            if name in self.variables[i]:
                return self.variables[i][name]
        raise Exception('No declaration for the variable %s' % name)

    def find_fun(self, name, argtypes):
        signature = (name, *argtypes)
        for i in reversed(range(len(self.functions))):
            if signature in self.functions[i]:
                return self.functions[i][signature]
        raise Exception('No declaration for the function %s' % signature[0], signature[1:])


# from syntree import *
# from symtable import *
def decorate(ast):
    if not isinstance(ast, Function) or ast.name != 'main' or ast.deco['type'] != Type.VOID or len(ast.args)>0:
        raise Exception('Cannot find a valid entry point')
    symtable = SymbolTable()
    symtable.add_fun(ast.name, [], ast.deco)
    ast.deco['strings'] = set() # collection of constant strings from the program
    process_scope(ast, symtable)
    process_scope(ast, symtable)
    ast.deco['scope_cnt'] = symtable.scope_cnt # total number of functions, necessary for the static scope display table allocation

def process_scope(fun, symtable):
    symtable.push_scope(fun.deco)
    for v in fun.args: # process function arguments
        symtable.add_var(v.name, v.deco)
    for v in fun.var:  # process local variables
        symtable.add_var(v.name, v.deco)
    for f in fun.fun:  # process nested functions: first add function symbols to the table
        symtable.add_fun(f.name, [v.deco['type'] for v in f.args], f.deco)
    for f in fun.fun:  # then process nested function bodies
        process_scope(f, symtable)
    for s in fun.body: # process the list of statements
        process_instruction(s, symtable)
    symtable.pop_scope()

def process_instruction(n, symtable):
    match n:
        case Print(): # no type checking is necessary
            process_instruction(n.expr, symtable)
        case Return():
            if n.expr is None: return # TODO semantic check for return; in non-void functions
            process_instruction(n.expr, symtable)
            if symtable.ret_stack[-1]['type'] != n.expr.deco['type']:
                raise Exception('Incompatible types in return statement, line %s', n.deco['lineno'])
        case Assign():
            process_instruction(n.expr, symtable)
            n.deco |= symtable.find_var(n.name)
            if n.deco['type'] != n.expr.deco['type']:
                raise Exception('Incompatible types in assignment statement, line %s', n.deco['lineno'])
        case While():
            process_instruction(n.expr, symtable)
            if n.expr.deco['type'] != Type.BOOL:
                raise Exception('Non-boolean expression in while statement, line %s', n.deco['lineno'])
            for s in n.body:
                process_instruction(s, symtable)
        case IfThenElse():
            process_instruction(n.expr, symtable)
            if n.expr.deco['type'] != Type.BOOL:
                raise Exception('Non-boolean expression in if statement, line %s', n.deco['lineno'])
            for s in n.ibody + n.ebody:
                process_instruction(s, symtable)
        case ArithOp():
            process_instruction(n.left,  symtable)
            process_instruction(n.right, symtable)
            if n.left.deco['type'] != Type.INT or n.right.deco['type'] != Type.INT:
                raise Exception('Arithmetic operation over non-integer type in line %s', n.deco['lineno'])
        case LogicOp():
            process_instruction(n.left,  symtable)
            process_instruction(n.right, symtable)
            if (n.left.deco['type'] != n.right.deco['type']) or \
               (n.op in ['<=', '<', '>=', '>'] and n.left.deco['type'] != Type.INT) or \
               (n.op in ['&&', '||'] and n.left.deco['type'] != Type.BOOL):
                raise Exception('Boolean operation over incompatible types in line %s', n.deco['lineno'])
        case Var(): # no type checking is necessary
            n.deco |= symtable.find_var(n.name)
        case FunCall():
            for s in n.args:
                process_instruction(s, symtable)
            n.deco |= symtable.find_fun(n.name, [ a.deco['type'] for a in n.args ])
        case String(): # no type checking is necessary
            symtable.ret_stack[1]['strings'].add((n.deco['label'], n.value))
        case Integer() | Boolean(): pass # no type checking is necessary
        case other: raise Exception('Unknown instruction', n)


# 5. transasm.py (including transasm_recipe.py)

templates = {
'ascii' : '''{label}: .ascii "{string}"
	{label}_len = . - {label}
''',
'var' : '''	movl display+{scope}, %eax
	movl -{variable}(%eax), %eax
''',
'print_linebreak' : '''	pushl $10           # '\\n'
	movl $4, %eax       # write system call
	movl $1, %ebx       # stdout
	leal 0(%esp), %ecx  # address of the character
	movl $1, %edx       # one byte
	int  $0x80          # make system call
	addl $4, %esp
''',
'print_int' : '''{expr}
	pushl %eax
	call print_int32
	addl $4, %esp
''',
'print_string' : '''	movl $4, %eax
	movl $1, %ebx
	movl ${label}, %ecx
	movl ${label}_len, %edx
	int  $0x80
''',
'print_bool' : '''{expr}
	movl $truestr, %ecx
	movl $truestr_len, %edx
	test %eax, %eax
	jnz 0f
	movl $falsestr, %ecx
	movl $falsestr_len, %edx
0:	movl $4, %eax
	movl $1, %ebx
	int  $0x80
''',
'assign' : '''{expression}
	pushl %eax
	movl display+{scope}, %eax
	popl %ebx
	movl %ebx, -{variable}(%eax)
''',
'ifthenelse' : '''{condition}
	test %eax, %eax
	jz {label1}
{ibody}
	jmp {label2}
{label1}:
{ebody}
{label2}:
''',
'while' : '''{label1}:
{condition}
	test %eax, %eax
	jz {label2}
{body}
	jmp {label1}
{label2}:
''',
'funcall' : '''	pushl display+{scope}
{allocargs}
	subl ${varsize}, %esp
	leal {disphead}(%esp), %eax
	movl %eax, display+{scope}
	call {funlabel}
	movl display+{scope}, %esp
	addl $4, %esp
	popl display+{scope}
''',
'program' : '''.global _start
	.data
{strings}
truestr: .ascii "true"
	truestr_len = . - truestr
falsestr: .ascii "false"
	falsestr_len = . - falsestr
	.align 2
display: .skip {display_size}
	.text
_start:
	leal -4(%esp), %eax
	movl %eax, display+{offset}
	subl ${varsize}, %esp # allocate locals
	call {main}
	addl ${varsize}, %esp # deallocate locals
_end:               # do not care about clearing the stack
	movl $1, %eax   # _exit system call (check asm/unistd_32.h for the table)
	movl $0, %ebx   # error code 0
	int $0x80       # make system call
{functions}
print_int32:
	movl 4(%esp), %eax  # the number to print
	cdq
	xorl %edx, %eax
	subl %edx, %eax     # abs(%eax)
	pushl $10           # base 10
	movl %esp, %ecx     # buffer for the string to print
	subl $16, %esp      # max 10 digits for a 32-bit number (keep %esp dword-aligned)
0:	xorl %edx, %edx     #     %edx = 0
	divl 16(%esp)       #     %eax = %edx:%eax/10 ; %edx = %edx:%eax % 10
	decl %ecx           #     allocate one more digit
	addb $48, %dl       #     %edx += '0'       # 0,0,0,0,0,0,0,0,0,0,'1','2','3','4','5','6'
	movb %dl, (%ecx)    #     store the digit   # ^                   ^                    ^
	test %eax, %eax     #                       # %esp                %ecx (after)         %ecx (before)
	jnz 0b              # until %eax==0         #                     <----- %edx = 6 ----->
	cmp %eax, 24(%esp)  # if the number is negative
	jge 0f
	decl %ecx           # allocate one more character
	movb $45, 0(%ecx)   # '-'
0:	movl $4, %eax       # write system call
	movl $1, %ebx       # stdout
	leal 16(%esp), %edx # the buffer to print
	subl %ecx, %edx     # number of digits
	int $0x80           # make system call
	addl $20, %esp      # deallocate the buffer
	ret
'''}

# from syntree import *
# from transasm_recipe import templates

def transasm(n):
    strings = ''.join([templates['ascii'].format(**locals()) for label,string in n.deco['strings']])
    display_size = n.deco['scope_cnt']*4
    offset       = n.deco['scope']*4
    main         = n.deco['label']
    varsize      = len(n.var)*4
    functions    = fun(n)
    return templates['program'].format(**locals())

def fun(n):
    label  = n.deco['label']
    nested = ''.join([ fun(f) for f in n.fun ])
    body   = ''.join([stat(s) for s in n.body])
    return f'{label}:\n{body}\n\tret\n{nested}\n'

def stat(n):
    match n:
        case Print():
            match n.expr.deco['type']:
                case Type.INT:
                    asm = templates['print_int'].format(expr = stat(n.expr))
                case Type.BOOL:
                    asm = templates['print_bool'].format(expr = stat(n.expr))
                case Type.STRING:
                    asm = templates['print_string'].format(label = n.expr.deco['label'])
                case other: raise Exception('Unknown expression type', n.expr)
            return asm + (templates['print_linebreak'] if n.newline else '')
        case Return():
            return (stat(n.expr) if n.expr is not None and n.expr.deco['type'] != Type.VOID else '') + '\tret\n'
        case Assign():
            return templates['assign'].format(expression = stat(n.expr),
                                                   scope = n.deco['scope']*4,
                                                variable = n.deco['offset']*4)
        case While():
            return templates['while'].format(condition = stat(n.expr),
                                                label1 = LabelFactory.new_label(),
                                                label2 = LabelFactory.new_label(),
                                                  body = ''.join([stat(s) for s in n.body]))
        case IfThenElse():
            return templates['ifthenelse'].format(condition = stat(n.expr),
                                                     label1 = LabelFactory.new_label(),
                                                     label2 = LabelFactory.new_label(),
                                                      ibody = ''.join([stat(s) for s in n.ibody]),
                                                      ebody = ''.join([stat(s) for s in n.ebody]))
        case ArithOp() | LogicOp():
            args = stat(n.left) + '\tpushl %eax\n' + stat(n.right) + '\tmovl %eax, %ebx\n\tpopl %eax\n'
            pyeq1 = {'+':'addl', '-':'subl', '*':'imull', '||':'orl', '&&':'andl'}
            pyeq2 = {'<=':'jle', '<':'jl', '>=':'jge', '>':'jg', '==':'je', '!=':'jne'}
            if n.op in pyeq1:
                return args + f'\t{pyeq1[n.op]} %ebx, %eax\n'
            elif n.op in pyeq2:
                return args + f'\tcmp %ebx, %eax\n\tmovl $1, %eax\n\t{pyeq2[n.op]} 1f\n\txorl %eax, %eax\n1:\n'
            elif n.op=='/':
                return args + '\tcdq\n\tidivl %ebx, %eax\n'
            elif n.op=='%':
                return args + '\tcdq\n\tidivl %ebx, %eax\n\tmovl %edx, %eax\n'
            raise Exception('Unknown binary operation')
        case Integer() | Boolean():
            return f'\tmovl ${int(n.value)}, %eax\n'
        case Var():
            return templates['var'].format(scope = n.deco['scope']*4, variable = n.deco['offset']*4)
        case FunCall():
            return templates['funcall'].format(allocargs = ''.join(['%s\tpushl %%eax\n' % stat(a) for a in n.args]),
                                                 varsize = n.deco['var_cnt']*4,
                                                disphead = n.deco['var_cnt']*4 + len(n.args)*4 - 4,
                                                   scope = n.deco['scope']*4,
                                                funlabel = n.deco['label'])
        case other: raise Exception('Unknown instruction', n)



# 6. final main part

import io, sys
# from lexer import WendLexer
# from parser import WendParser
# from analyzer import decorate
# from transasm import transasm

if len(sys.argv)!=2:
    sys.exit('Usage: compiler.py path/source.wend')
try:
    f = open(sys.argv[1], 'r')
    tokens = WendLexer().tokenize(f.read())
    ast = WendParser().parse(tokens)
    decorate(ast)
    asm_code = transasm(ast)
    # print(asm_code)
    # output to ./out.s
    with open('out.s', 'w') as out_file:
        out_file.write(asm_code)
    print('Assembly code generated in out.s')
    # as --march=i386 --32 -o out.o out.s && ld -m elf_i386 out.o -o out && ./out

except Exception as e:
    print(e)







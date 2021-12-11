'''
Frontend for our language - builds an AST where each
node is of the shape,

    (TYPE, [arg1, arg2, arg3,...])

here TYPE is a string describing the node type.
'''

# the job of a fe is to orginize the syntex of the grammar, it will not store any value to symtab nor will it get any values from the symtab

# AKA the parser


'''
Frontend for our Cuppa5 language - builds an AST where each
node is of the shape,

    (TYPE, [arg1, arg2, arg3,...])

here TYPE is a string describing the node type.
'''
# helper function to compute the type of a function
def formalargs_type(args):

    output_list = list()
    for a in args[1]:
        (FORMALARG, type, id) = a
        output_list.append(type)
    return ('LIST', output_list)

# there are lots of places where we need to lookahead
# on primitive data types -- this list will make it easier.
primitive_lookahead = [
    'BOOL_TYPE',
    'INT_TYPE',
    'FLOAT_TYPE',
    'FRACT_TYPE',
    'STRING_TYPE',
    ]

exp_lookahead = [
    'BOOL',
    'INT',
    'FLOAT',
    'FRACT',
    'STRING',
    'ID',
    'LPAREN',
    'MINUS',
    'NOT',
    'IN',
    'LBRACKET',
]

# stmt_list : ({VOID_TYPE,INT_TYPE,FLOAT_TYPE,STRING_TYPE,ID,GET,OUT,RETURN,WHILE,IF,LCURLY} stmt)*
stmt_lookahead = [
    'BOOL_TYPE',
    'INT_TYPE',
    'FLOAT_TYPE',
    'FRACT_TYPE',
    'STRING_TYPE',
    'VOID_TYPE',
    'ID',
    'OUT',
    'RETURN',
    'WHILE',
    'IF',
    'FOR',
    'DO',
    ]

def stmt_list(stream):
    lst = []
    while stream.pointer().type in stmt_lookahead:
        s = stmt(stream)
        lst.append(s)
    return ('STMTLIST', lst)

# stmt :  {VOID_TYPE} VOID_TYPE ID LPAREN formal_args? RPAREN stmt
#      |  {BOOL_TYPE,INT_TYPE,FLOAT_TYPE,FRACT_TYPE,STRING_TYPE} data_type ID decl_suffix?
#      |  {ID} ID id_suffix
#      |  {OUT} OUT exp
#      |  {RETURN} RETURN exp?
#      |  {WHILE} WHILE exp stmt
#      |  {IF} IF exp stmt (ELSE stmt)?
#      |  {FOR} FOR (INT_TYPE? ID ASSIGN)? exp COLON exp COLON exp stmt
#      |  {DO} DO stmt_list END
def stmt(stream):
    if stream.pointer().type in ['VOID_TYPE']:
        stream.match('VOID_TYPE')
        ret_type = ('VOID_TYPE',)
        id_tok = stream.match('ID')
        stream.match('LPAREN')
        args = ('NIL',)
        if stream.pointer().type in primitive_lookahead:
            args = formal_args(stream)
        stream.match('RPAREN')
        arg_types = formalargs_type(args)
        body = stmt(stream)
        return ('FUNC_DECL', 
                ('ID', id_tok.value), 
                ('FUNC_TYPE', ret_type, arg_types), 
                args, 
                body)
    elif stream.pointer().type in primitive_lookahead:
        type = data_type(stream)
        id_tok = stream.match('ID')
        # if there is a decl_suffix
        if stream.pointer().type in ['LPAREN', 'ASSIGN']:
            e = decl_suffix(stream)
            if e[0] == 'FUNCTION':
                (FUNCTION, args, body) = e
                arg_types = formalargs_type(args)
                return ('FUNC_DECL',
                        ('ID', id_tok.value),
                        ('FUNC_TYPE', type, arg_types),
                        args,
                        body)
            elif e[0] == 'EXP_INIT':
                return ('VAR_DECL',
                        ('ID', id_tok.value),
                        type,
                        e[1])
            elif e[0] == 'ARR_INIT':
                return ('ARR_DECL',
                        ('ID', id_tok.value),
                        type,
                        e[1])
        # else if there is no decl_suffix
        else:
            return ('VAR_DECL',
                    ('ID', id_tok.value),
                    type,
                    None)
    elif stream.pointer().type in ['ID']:
        id_tok = stream.match('ID')
        e = id_suffix(stream)
        if e[0] == 'CALL':
            return ('CALL_STMT', ('ID', id_tok.value), e[1])
        elif e[0] == 'VAR_ASSIGN':
            return ('ASSIGN', ('ID', id_tok.value), e[1])
        elif e[0] == 'ARR_ASSIGN':
            return ('ASSIGN', ('ARR_ACCESS', ('ID', id_tok.value), ('IX', e[1])), e[2])
        elif e[0] == 'FUNC_ARR_ASSIGN':
            (FUN_ARR_ASSIGN, args, ix, ae) = e
            return ('ASSIGN', ('ARR_ACCESS', ('CALL_EXP', ('ID', id_tok.value), args), ('IX', ix)), ae)
    elif stream.pointer().type in ['OUT']:
        stream.match('OUT')
        e = exp(stream)
        return ('OUT', e)
    elif stream.pointer().type in ['RETURN']:
        stream.match('RETURN')
        e = ('NIL',)
        if stream.pointer().type in exp_lookahead:
            e = exp(stream)
        return ('RETURN', e)
    elif stream.pointer().type in ['WHILE']:
        stream.match('WHILE')
        cond = exp(stream)
        body = stmt(stream)
        return ('WHILE', cond, body)
    elif stream.pointer().type in ['IF']:
        stream.match('IF')
        cond = exp(stream)
        body = stmt(stream)
        if stream.pointer().type == 'ELSE':
            stream.match('ELSE')
            else_body = stmt(stream)
            return ('IF', cond, body, else_body)
        else:
            return ('IF', cond, body, ('NIL',))
    elif stream.pointer().type in ['FOR']:
        #todo this only allows the syntax: for 1:10:2
        #todo allow the syntax: for x <- 1:10:2
        #todo allow the syntax: for int x <- 1:10:2
        stream.match('FOR')
        range_start = exp(stream)
        stream.match('COLON')
        range_end = exp(stream)
        stream.match('COLON')
        increment = exp(stream)
        body = stmt(stream)
        return ('FOR', range_start, range_end, increment, body)
    elif stream.pointer().type in ['DO']:
        stream.match('DO')
        e = stmt_list(stream)
        stream.match('END')
        return ('BLOCK', e)
    else:
        raise SyntaxError("stmt: syntax error at {}".format(stream.pointer().value))

# data_type :  primitive_type ARR_TYPE?
def data_type(stream):
    if stream.pointer().type in primitive_lookahead:
        type = primitive_type(stream)
        if stream.pointer().type in ['ARR_TYPE']:
            stream.match('ARR_TYPE')
            return ('ARR_TYPE', type)
        return type
    else:
        raise SyntaxError("data_type: syntax error at {}".format(stream.pointer().value))

# primitive_type :  {BOOL_TYPE} BOOL_TYPE
#                |  {INT_TYPE} INT_TYPE
#                |  {FLOAT_TYPE} FLOAT_TYPE
#                |  {FRACT_TYPE} FRACT_TYPE
#                |  {STRING_TYPE} STRING_TYPE
def primitive_type(stream):
    if stream.pointer().type in ['BOOL_TYPE']:
        stream.match('BOOL_TYPE')
        return ('BOOL_TYPE',)
    elif stream.pointer().type in ['INT_TYPE']:
        stream.match('INT_TYPE')
        return ('INT_TYPE',)
    elif stream.pointer().type in ['FLOAT_TYPE']:
        stream.match('FLOAT_TYPE')
        return ('FLOAT_TYPE',)
    elif stream.pointer().type in ['FRACT_TYPE']:
        stream.match('FRACT_TYPE')
        return ('FRACT_TYPE',)
    elif stream.pointer().type in ['STRING_TYPE']:
        stream.match('STRING_TYPE')
        return ('STRING_TYPE',)
    else:
        raise SyntaxError("primitive_type: syntax error at {}".format(stream.pointer().value))

# decl_suffix :  {LPAREN} LPAREN formal_args? RPAREN stmt
#             |  {ASSIGN} ASSIGN exp
def decl_suffix(stream):
    if stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        if stream.pointer().type in primitive_lookahead:
            args = formal_args(stream)
        else:
            args = ('LIST', [])
        stream.match('RPAREN')
        body = stmt(stream)
        return ('FUNCTION', args, body)
    elif stream.pointer().type in ['ASSIGN']:
        stream.match('ASSIGN')
        if stream.pointer().type in exp_lookahead:
            ie = exp(stream)
            if ie[0] == 'CONST':
                e = ('EXP_INIT', ie)
            elif ie[0] == 'LIST':
                e = ('ARR_INIT', ie)
            else:
                raise ValueError("only constants are allowed in initializers")
            return e
    else:
        raise SyntaxError("id_suffix: syntax error at {}".format(stream.pointer().value))

#todo fix
# id_suffix :  {LPAREN} LPAREN actual_args? RPAREN (LBRACKET exp RBRACKET ASSIGN exp)?
#           |  {LBRACKET} LBRACKET exp RBRACKET (LPAREN actual_args? RPAREN)?
#           |  {ASSIGN} ASSIGN exp
def id_suffix(stream):
    if stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        args = ('LIST', [])
        if stream.pointer().type in exp_lookahead:
            args = actual_args(stream)
        stream.match('RPAREN')
        tree = ('CALL', args)
        if stream.pointer().type in ['LBRACKET']:
            stream.match('LBRACKET')
            ix = exp(stream)
            stream.match('RBRACKET')
            stream.match('ASSIGN')
            e = exp(stream)
            tree = ('FUNC_ARR_ASSIGN', args, ix, e)
        return tree
    elif stream.pointer().type in ['LBRACKET']:
        stream.match('LBRACKET')
        ix = exp(stream)
        stream.match('RBRACKET')
        stream.match('ASSIGN')
        e = exp(stream)
        return ('ARR_ASSIGN', ix, e)
    elif stream.pointer().type in ['ASSIGN']:
        stream.match('ASSIGN')
        e = exp(stream)
        return ('VAR_ASSIGN', e)
    else:
        raise SyntaxError("id_suffix: syntax error at {}".format(stream.pointer().value))


# exp :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp_low
def exp(stream):
    if stream.pointer().type in exp_lookahead:
        e = exp_low(stream)
        return e
    else:
        raise SyntaxError("exp: syntax error at {}"
                          .format(stream.pointer().value))

# exp_low :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp_med ((EQ|LE) exp_med)*
def exp_low(stream):
    if stream.pointer().type in exp_lookahead:
        e = exp_med(stream)
        while stream.pointer().type in ['EQ', 'LE']:
            if stream.pointer().type == 'EQ':
                op_tk = stream.match('EQ')
            else:
                op_tk = stream.match('LE')
            tmp = exp_med(stream)
            e = (op_tk.type, e, tmp)
        return e
    else:
        raise SyntaxError("exp_low: syntax error at {}".format(stream.pointer().value))

# exp_med :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp_high ((MUL|DIV) exp_high)*
def exp_med(stream):
    if stream.pointer().type in exp_lookahead:
        e = exp_high(stream)
        while stream.pointer().type in ['PLUS', 'MINUS']:
            if stream.pointer().type == 'PLUS':
                op_tk = stream.match('PLUS')
            else:
                op_tk = stream.match('MINUS')
            tmp = exp_high(stream)
            e = (op_tk.type, e, tmp)
        return e
    else:
        raise SyntaxError("exp_med: syntax error at {}".format(stream.pointer().value))

# exp_high :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} primary ((MUL|DIV) primary)*
def exp_high(stream):
    if stream.pointer().type in exp_lookahead:
        e = primary(stream)
        while stream.pointer().type in ['MUL', 'DIV']:
            if stream.pointer().type == 'MUL':
                op_tk = stream.match('MUL')
            else:
                op_tk = stream.match('DIV')
            tmp = primary(stream)
            e = (op_tk.type, e, tmp)
        return e
    else:
        raise SyntaxError("exp_high: syntax error at {}".format(stream.pointer().value))

# primary :  {BOOL} BOOL
#         |  {INT} INT
#         |  {FLOAT} FLOAT
#         |  {FRACT} FRACT
#         |  {STRING} STRING
#         |  {ID} ID ({LPAREN,LBRACKET} id_exp_suffix)?
#         |  {LPAREN} LPAREN exp RPAREN
#         |  {MINUS} MINUS primary
#         |  {NOT} NOT primary
#         |  {IN} IN STRING?
#         |  {LBRACKET} LBRACKET (exp (COMMA exp)*)? RBRACKET
def primary(stream):
    if stream.pointer().type in ['BOOL']:
        tk = stream.match('BOOL')
        return ('CONST', ('BOOL_TYPE',), ('VALUE', bool(tk.value)))
    elif stream.pointer().type in ['INT']:
        tk = stream.match('INT')
        return ('CONST', ('INT_TYPE',), ('VALUE', int(tk.value)))
    elif stream.pointer().type in ['FLOAT']:
        tk = stream.match('FLOAT')
        return ('CONST', ('FLOAT_TYPE',), ('VALUE', float(tk.value)))
    #todo FRACT
    elif stream.pointer().type in ['STRING']:
        tk = stream.match('STRING')
        return ('CONST', ('STRING_TYPE',), ('VALUE', str(tk.value)))
    elif stream.pointer().type in ['ID']:
        id_tok = stream.match('ID')
        if stream.pointer().type in ['LPAREN','LBRACKET']:
            e = id_exp_suffix(stream)
            if e[0] == 'CALL':
                return ('CALL_EXP', ('ID', id_tok.value), e[1])
            elif e[0] == 'ARRAY':
                return ('ARR_ACCESS', ('ID', id_tok.value), ('IX', e[1]))
            elif e[0] == 'FUNC_ARR':
                return ('ARR_ACCESS', ('CALL_EXP', ('ID', id_tok.value), e[1]), ('IX', e[2]))
            else:
                raise ValueError("unknown suffix {}".format(e[0]))
        else:
            return ('ID', id_tok.value)
    elif stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        e = exp(stream)
        stream.match('RPAREN')
        return e
    elif stream.pointer().type in ['MINUS']:
        stream.match('MINUS')
        e = primary(stream)
        if e[0] == 'CONST' and e[1][0] in ['INT_TYPE', 'FLOAT_TYPE']:
            return ('CONST', e[1], -e[2])
        #todo elif e[0] == 'CONST' and e[1][0] in ['FRACT_TYPE']?
        else:
            return ('UMINUS', e)
    elif stream.pointer().type in ['NOT']:
        stream.match('NOT')
        e = primary(stream)
        # (CONST, TYPE, VAL)
        if e[0] == 'CONST' and e[1][0] == 'BOOL_TYPE': 
            return ('CONST', ('BOOL_TYPE',), False if e[2] else True)
        else:
            return ('NOT', e)
    elif stream.pointer().type in ['IN']:
        stream.match('IN')
        if stream.pointer().type in ['STRING']:
            tk = stream.match('STRING')
            return ('IN', ('STRING', tk.value))
        return ('IN', ('STRING', ''))
    elif stream.pointer().type in ['LBRACKET']:
        stream.match('LBRACKET')
        if stream.pointer().type in exp_lookahead:
            arr = [exp(stream)]
            while stream.pointer().type in ['COMMA']:
                stream.match('COMMA')
                arr.append(exp(stream))
        else:
            raise ValueError("unknown value {}".format(stream.pointer().value))
        stream.match('RBRACKET')
        return ('ARR', arr)
    else:
        raise SyntaxError("primary: syntax error at {}".format(stream.pointer().value))

# id_exp_suffix :  {LPAREN} LPAREN actual_args? RPAREN (LBRACKET exp RBRACKET)?
#               |  {LBRACKET} LBRACKET exp RBRACKET
def id_exp_suffix(stream):
    if stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        if stream.pointer().type in ['BOOL','INT','FLOAT','FRACT','STRING','ID','LPAREN','MINUS','NOT','IN','LBRACKET']:
            args = actual_args(stream)
        else:
            args = ('LIST', [])
        stream.match('RPAREN')
        tree = ('CALL', args)
        if stream.pointer().type in ['LBRACKET']:
            stream.match('LBRACKET')
            e = exp(stream)
            stream.match('RBRACKET')
            tree = ('FUNC_ARRAY', args, e)
        return tree
    elif stream.pointer().type() in ['LBRACKET']:
        stream.match('LBRACKET')
        e = exp(stream)
        stream.match('RBRACKET')
        return ('ARR', e)
    else:
        raise ValueError("syntax error at {}".format(stream.pointer().value))

# {BOOL_TYPE,INT_TYPE,FLOAT_TYPE,FRACT_TYPE,STRING_TYPE} data_type ID (COMMA data_type ID)*
def formal_args(stream):
    if stream.pointer().type in ['BOOL_TYPE','INT_TYPE','FLOAT_TYPE','FRACT_TYPE','STRING_TYPE']:
        type = data_type(stream)
        id_tok = stream.match('ID')
        arglist = [('FORMALARG', type, ('ID', id_tok.value))]
        while stream.pointer().type in ['COMMA']:
            stream.match('COMMA')
            type = data_type(stream)
            id_tok = stream.match('ID')
            arglist.append(('FORMALARG', type, ('ID', id_tok.value)))
        return ('LIST', arglist)

# {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp (COMMA exp)*
def actual_args(stream):
    if stream.pointer().type in ['BOOL','INT','FLOAT','FRACT','STRING','ID','LPAREN','MINUS','NOT','IN','LBRACKET']:
        e = exp(stream)
        arglist = [e]
        while stream.pointer().type in ['COMMA']:
            stream.match('COMMA')
            e = exp(stream)
            arglist.append(e)
        return ('LIST', arglist)
    else:
        raise SyntaxError("actual_args: syntax error at {}".format(stream.pointer().value))

# frontend top-level driver
def parse(stream):
    from lexer import Lexer
    token_stream = Lexer(stream)
    sl = stmt_list(token_stream) # call the parser function for start symbol
    if not token_stream.end_of_file():
        raise SyntaxError("parse: syntax error at {}"
                          .format(token_stream.pointer().value))
    else:
        return sl

if __name__ == "__main__":
    from sys import stdin
    from dumpast import dumpast
    char_stream = stdin.read() # read from stdin
    dumpast(parse(char_stream))

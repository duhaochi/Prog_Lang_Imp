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
    'FUNC',
    'ID',
    'OUT',
    'RETURN',
    'WHILE',
    'IF',
    'FOR',
    'DO',
]


# stmt_list : (stmt)*
def stmt_list(stream):
    lst = []
    while stream.pointer().type in stmt_lookahead:
        s = stmt(stream)
        lst.append(s)
    return ('STMTLIST', lst)


# stmt :  {FUNC} FUNC data_type ID LPAREN formal_args? RPAREN stmt
#      |  {BOOL_TYPE,INT_TYPE,FLOAT_TYPE,FRACT_TYPE,STRING_TYPE} data_type ID ASSIGN exp
#      |  {ID} ID id_suffix
#      |  {OUT} OUT exp
#      |  {RETURN} RETURN exp
#      |  {WHILE} WHILE exp stmt
#      |  {IF} IF exp stmt (ELSE stmt)?
#      |  {FOR} FOR exp COLON exp COLON exp stmt
#      |  {DO} DO stmt_list END
def stmt(stream):
    if stream.pointer().type in ['FUNC']:
        stream.match('FUNC')
        ret_type = data_type(stream)
        id_tk = stream.match('ID')
        stream.match('LPAREN')
        if stream.pointer().type in primitive_lookahead:
            args = formal_args(stream)
        else:
            # args = ('LIST', [])
            args = ('NIL',)
        arg_types = formalargs_type(args)
        stream.match('RPAREN')
        body = stmt(stream)
        # return ('FUNDECL', ('ID', id_tk.value), dtype, args, body)
        return ('FUNDECL',
                ('ID', id_tk.value),
                ('FUNC_TYPE', ret_type, arg_types),
                args,
                body)
    elif stream.pointer().type in primitive_lookahead:
        dtype = data_type(stream)
        id_tk = stream.match('ID')
        stream.match('ASSIGN')
        e = exp(stream)
        return ('VARDECL', ('ID', id_tk.value), dtype, e)
    elif stream.pointer().type in ['ID']:
        id_tk = stream.match('ID')
        suffix = id_suffix(stream)
        if suffix[0] == 'CALL_STMT':
            # note suffix = ('CALL_STMT', actual_args)
            return ('CALL_STMT', ('ID', id_tk.value), suffix[1])
        elif suffix[0] == 'ASSIGN':
            # note suffix = ('ASSIGN', exp)
            return ('ASSIGN', ('ID', id_tk.value), suffix[1])
    elif stream.pointer().type in ['OUT']:
        stream.match('OUT')
        e = exp(stream)
        return ('OUT', e)
    elif stream.pointer().type in ['RETURN']:
        stream.match('RETURN')
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
        if stream.pointer().type in ['ELSE']:
            stream.match('ELSE')
            elsebody = stmt(stream)
        else:
            elsebody = ('NIL',)
        return ('IF', cond, body, elsebody)
    elif stream.pointer().type in ['FOR']:
        stream.match('FOR')
        range_start = exp(stream)
        stream.match('COLON')
        range_end = exp(stream)
        stream.match('COLON')
        range_inc = exp(stream)
        body = stmt(stream)
        return ('FOR', range_start, range_end, range_inc, body)
    elif stream.pointer().type in ['DO']:
        stream.match('DO')
        body = stmt_list(stream)
        stream.match('END')
        return ('BLOCK', body)
    else:
        raise SyntaxError("data_type: syntax error at {}".format(stream.pointer().value))


# data_type :  {BOOL_TYPE} BOOL_TYPE
#           |  {INT_TYPE} INT_TYPE
#           |  {FLOAT_TYPE} FLOAT_TYPE
#           |  {FRACT_TYPE} FRACT_TYPE
#           |  {STRING_TYPE} STRING_TYPE
def data_type(stream):
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
        raise SyntaxError("data_type: syntax error at {}".format(stream.pointer().value))


# id_suffix :  {LPAREN} LPAREN actual_args? RPAREN
#           |  {ASSIGN} ASSIGN exp
def id_suffix(stream):
    if stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        if stream.pointer().type in exp_lookahead:
            args = actual_args(stream)
        else:
            args = ('LIST', [])
        stream.match('RPAREN')
        return ('CALL_STMT', args)
    elif stream.pointer().type in ['ASSIGN']:
        stream.match('ASSIGN')
        e = exp(stream)
        return ('ASSIGN', e)
    else:
        raise SyntaxError("id_suffix: syntax error at {}"
                          .format(stream.pointer().value))


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


# exp_med :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp_high ((PLUS|MINUS) exp_high)*
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
#         |  {ID} ID (LPAREN actual_args? RPAREN)?  //CALL_EXP//
#         |  {LPAREN} LPAREN exp RPAREN
#         |  {MINUS} MINUS primary
#         |  {NOT} NOT primary
#         |  {IN} IN STRING?
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
    # todo FRACT
    elif stream.pointer().type in ['STRING']:
        tk = stream.match('STRING')
        return ('CONST', ('STRING_TYPE',), ('VALUE', str(tk.value)))
    elif stream.pointer().type in ['ID']:
        id_tk = stream.match('ID')
        # if LPAREN: // CALL_EXP //
        if stream.pointer().type in ['LPAREN']:
            stream.match('LPAREN')
            if stream.pointer().type in exp_lookahead:
                args = actual_args(stream)
            else:
                args = ('LIST', [])
            stream.match('RPAREN')
            return ('CALL_EXP', ('ID', id_tk.value), args)
        # else: // ID_EXP //
        else:
            return ('ID', id_tk.value)
    elif stream.pointer().type in ['LPAREN']:
        stream.match('LPAREN')
        e = exp(stream)
        stream.match('RPAREN')
        return e
    elif stream.pointer().type in ['MINUS']:
        stream.match('MINUS')
        if stream.pointer().type == 'INT':
            tk = stream.match('INT')
            return ('CONST', ('INT_TYPE',), ('VALUE', 0 - int(tk.value)))
        elif stream.pointer().type == 'FLOAT':
            tk = stream.match('FLOAT')
            return ('CONST', ('FLOAT_TYPE',), ('VALUE', 0 - float(tk.value)))
    elif stream.pointer().type in ['NOT']:
        stream.match('NOT')
        if stream.pointer().type in ['INT', 'FLOAT']:
            v = 'True' if stream.pointer().value == 0 else 'False'
        elif stream.pointer().type in ['BOOL']:
            v = 'False' if stream.pointer().value == 'True' else 'True'
        else:
            raise SyntaxError("not operator expects a bool-compatible value")
        return ('CONST', ('BOOL_TYPE',), ('VALUE', v))
    else:
        raise SyntaxError("primary: syntax error at {}".format(stream.pointer().value))


#
# formal_args :  {BOOL_TYPE,INT_TYPE,FLOAT_TYPE,FRACT_TYPE,STRING_TYPE} data_type ID (COMMA data_type ID)*
def formal_args(stream):
    if stream.pointer().type in ['BOOL_TYPE', 'INT_TYPE', 'FLOAT_TYPE', 'FRACT_TYPE', 'STRING_TYPE']:
        type = data_type(stream)
        id_tok = stream.match('ID')
        arglist = [('FORMALARG', type, ('ID', id_tok.value))]
        while stream.pointer().type in ['COMMA']:
            stream.match('COMMA')
            type = data_type(stream)
            id_tok = stream.match('ID')
            arglist.append(('FORMALARG', type, ('ID', id_tok.value)))
        return ('LIST', arglist)
    else:
        raise SyntaxError("actual_args: syntax error at {}".format(stream.pointer().value))


# actual_args :  {BOOL,INT,FLOAT,FRACT,STRING,ID,LPAREN,MINUS,NOT,IN,LBRACKET} exp (COMMA exp)*
def actual_args(stream):
    if stream.pointer().type in exp_lookahead:
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
    sl = stmt_list(token_stream)  # call the parser function for start symbol
    if not token_stream.end_of_file():
        raise SyntaxError("parse: syntax error at {}"
                          .format(token_stream.pointer().value))
    else:
        return sl


if __name__ == "__main__":
    from sys import stdin
    from dumpast import dumpast

    char_stream = stdin.read()  # read from stdin
    dumpast(parse(char_stream))

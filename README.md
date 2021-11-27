# Prog_Lang_Imp
This is a programming language implementation by Haochi &amp; Andy


stmt_list : (stmt)*

stmt :  data_type ID decl_suffix?
     |  VOID_TYPE ID LPAREN formal_args? RPAREN stmt
     |  ID id_suffix
     |  OUT exp
     |  RETURN exp?
     |  WHILE exp stmt
     |  IF exp stmt (ELSE stmt)?
     |  FOR (INT_TYPE? ID ASSIGN)? exp COLON exp COLON exp stmt
     |  stmt_list

data_type :  BOOL_TYPE ARR_TYPE?
          |  INT_TYPE ARR_TYPE?
          |  FLOAT_TYPE ARR_TYPE?
          |  FRACT_TYPE ARR_TYPE?
          |  STRING_TYPE ARR_TYPE?

decl_suffix :  LPAREN formal_args? RPAREN stmt
            |  ASSIGN exp

id_suffix :  LPAREN actual_args? RPAREN (LBRACKET exp RBRACKET ASSIGN exp)?
          |  LBRACKET exp RBRACKET (LPAREN actual_args? RPAREN)?
          |  ASSIGN exp

exp :  exp_low
exp_low :  exp_med ((EQ|LE) exp_med)*
exp_med :  exp_high ((PLUS|MINUS) exp_high)*
exp_high :  primary ((MUL|DIV) primary)*

primary :  BOOL
        |  INT
        |  FLOAT
        |  FRACT
        |  STRING
        |  ID (LPAREN actual_args? RPAREN)?
        |  LPAREN exp RPAREN
        |  MINUS primary
        |  NOT primary
        |  INPUT STRING?
        |  LBRACKET (exp (COMMA exp)*)? RBRACKET

formal_args :  data_type ID (COMMA data_type ID)*
actual_args :  exp (COMMA exp)*

ID :  <any valid variable name>
BOOL :  True | False
INT :  <any valid int number>
FLOAT :  <any valid floating point number>
FRACT :  <any valid fractional number>
STRING :  <any valid quoted str constant>

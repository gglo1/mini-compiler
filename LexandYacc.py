import ply.lex as lex
import ply.yacc as yacc
import math

#our tokens
symbol_table={}
reserved = {
   'if' : 'IF',
   'then' : 'THEN',
   'else' : 'ELSE',
   'while' : 'WHILE',
}
tokens = ( 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'LPAREN', 'RPAREN', 'POWER', 'SIN', 'COS', 'TAN', 'LN', 'LOG', 'EXP',
          'EQ', 'NEQ', 'LT', 'GT', 'ID',) + tuple(reserved.values())

# Regular expression rules for simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_POWER = r'\^'

t_EQ = r'=='
t_NEQ = r'!='
t_LT = r'<'
t_GT = r'>'

t_SIN = r'sin'
t_COS = r'cos'
t_TAN = r'tan'
t_LN = r'ln'
t_LOG = r'log'
t_EXP = r'exp'


#our rules for our tokens
#creation of a token
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t
    
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

#track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t' #we ignore white spaces

#incorrect character
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

#precedence rules for operators to handle ambiguity
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'POWER')
)


#creates our lexer
#lexer = lex.lex()

#data = input('calc >')

# Give the lexer some input
#lexer.input(data)

#1. Tokenize, handle lexical analysis
#while True:
    #tok = lexer.token()
    #if not tok: 
        #break      # No more input
    #print(tok)

#2. handle syntax analysis

def p_expression_plus(p):
    'expression : expression PLUS term'
    p[0] = p[1] + p[3]

def p_expression_minus(p):
    'expression : expression MINUS term'
    p[0] = p[1] - p[3]

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

def p_term_times(p):
    'term : term TIMES factor'
    p[0] = p[1] * p[3]

def p_term_div(p):
    'term : term DIVIDE factor'
    p[0] = p[1] / p[3]

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_num(p):
    'factor : NUMBER'
    p[0] = p[1]

def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]

def p_factor_power(p):
    'factor : factor POWER factor'
    p[0] = p[1] ** p[3]

def p_expression_equal(p):
    'expression : expression EQ expression'
    p[0] = p[1] == p[3]
   
def p_ID_equal(p):
    'expression : ID EQ expression'
    symbol_table[p[1]] = p[3]
    p[0] = f"Assigned value {p[3]} to identifier {p[1]}"
    print(symbol_table)

def p_expression_not_equal(p):
    'expression : expression NEQ expression'
    p[0] = p[1] != p[3]

def p_expression_less_than(p):
    'expression : expression LT expression'
    p[0] = p[1] < p[3]

def p_expression_greater_than(p):
    'expression : expression GT expression'
    p[0] = p[1] > p[3]

def p_factor_sin(p):
    'factor : SIN LPAREN expression RPAREN'
    trig = p[1].lower()
    angle = p[3]

    #check if degrees or radians
    if isinstance(angle, float) or isinstance(angle, int):
        angle = math.radians(angle)

    if trig == 'sin':
        p[0] = math.sin(angle)

def p_factor_cos(p):
    'factor : COS LPAREN expression RPAREN'
    trig = p[1].lower()
    angle = p[3]

    #check if degrees or radians
    if isinstance(angle, float) or isinstance(angle, int):
        angle = math.radians(angle)

    if trig == 'cos':
        p[0] = math.cos(angle)

def p_factor_tan(p):
    'factor : TAN LPAREN expression RPAREN'
    trig = p[1].lower()
    angle = p[3]

    #check if degrees or radians
    if isinstance(angle, float) or isinstance(angle, int):
        angle = math.radians(angle)

    if trig == 'tan':
        p[0] = math.tan(angle)

def p_factor_ln(p):
    'factor : LN LPAREN expression RPAREN'
    p[0] = math.log(p[3])

def p_factor_log(p):
    'factor : LOG LPAREN expression RPAREN'
    p[0] = math.log10(p[3])

def p_factor_exp(p):
    'factor : EXP LPAREN expression RPAREN'
    p[0] = math.exp(p[3])

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# Build the parser
#parser = yacc.yacc()

#main function for the syntax analysis
def main():

    #creates our lexer
    lexer = lex.lex()

    data = input('calc > ')

    # lexical analysis 
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
    #syntax analysis

    # Build the parser
    parser = yacc.yacc()

    result = parser.parse(data)
    print(result)

if __name__ == "__main__":
    main()

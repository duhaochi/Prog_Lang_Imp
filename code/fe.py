'''
Frontend for our language - builds an AST where each
node is of the shape,

    (TYPE, [arg1, arg2, arg3,...])

here TYPE is a string describing the node type.
'''

# the job of a fe is to orginize the syntex of the grammar, it will not store any value to symtab nor will it get any values from the symtab

# AKA the parser
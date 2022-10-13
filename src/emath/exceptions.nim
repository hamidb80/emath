type
    EMathException* = object of ValueError

    EMathLexError* = object of EMathException
    EMathParseError* = object of EMathException

    EMathEvalError* = object of EMathException
    EMathNotDefined* = object of EMathEvalError

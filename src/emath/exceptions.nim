type
    EMathException* = object of ValueError

    EMathParseError* = object of EMathException
    EMathTokenError* = object of EMathParseError
        slice*: Slice[int]

    EMathEvalError* = object of EMathException
    EMathNotDefined* = object of EMathEvalError

type
    EMathException* = object of ValueError

    EMathParseError* = object of EMathException
        slice*: Slice[int]

    EMathEvalError* = object of EMathException
    EMathNotDefined* = object of EMathEvalError

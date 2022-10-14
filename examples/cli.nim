## this is a simple cli math parser and evaluator using `emath`
##
## by entering math expression you get its final calculated value
##
## you can define a variable by:
##   <ident> = <expression>
## like:
##   hpi = sin(PI/2)
##
## you can also define a function by:
##   <ident>(...<ident>) = <expression>
## like:
##   sum(x, y) = x + y
##


import std/[tables, sugar]
import ../src/emath, ../src/emath/defaults


var
    fns = defaultfns
    vars = defaultvars

while true:
    stdout.write ">> "
    let input = stdin.readline

    if input != "":
        try:
            let ast = parse input

            if ast.kind == emnkInfix and ast.operator == emoAssign:
                case ast.left.kind
                of emnkVar:
                    vars[ast.left.ident] = ast.right.eval(vars, fns)

                of emnkCall:
                    let
                        fnDef = ast.left
                        fnName = fndef.ident
                        fnBody = ast.right
                        params = block:
                            var acc: seq[string]
                            for arg in fndef.children:
                                if arg.kind == emnkVar:
                                    acc.add arg.ident
                                else:
                                    raise newException(EMathEvalError, "parameters of a function must be raw idents")
                            acc

                    capture params, fnBody:
                        fns[fnName] = proc(args: seq[float]): float =
                            {.cast(noSideEffect).}:
                                assert args.len == params.len:
                                    "expected " & $params.len &
                                    " but given " &
                                    $args.len & " arguments"

                                for i, p in params:
                                    vars[p] = args[i]

                                result = fnBody.eval(vars, fns)

                                for p in params:
                                    del vars, p


                else:
                    echo "Logic Error: Left side of assignment must be a raw ident"

            else:
                echo "<< ", ast.eval(vars, fns)

        except EMathParseError:
            echo "Parser Error: " & getCurrentExceptionMsg()

        except EMathEvalError:
            echo "Evaluation Error: " & getCurrentExceptionMsg()

        except:
            echo "Unexpected Error: " & getCurrentExceptionMsg()

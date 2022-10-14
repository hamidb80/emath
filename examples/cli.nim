import std/[tables]
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

            if ast.kind == emnkInfix and ast.operator == emokAssign:
                if ast.left.kind == emnkVar:
                    vars[ast.left.ident] = ast.right.eval(vars, fns)
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

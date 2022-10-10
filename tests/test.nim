import std/unittest
import emath


when isMainModule:
  for expr in [
    "1 + 2", 
    "-1 * -2", 
    "1 + 2 * 3", 
    "1 * 2 + 3", 
    "--1 - 2 - 3",
    "1 ^ 8 + -3 * 7 < 1",
    "(1) + (2 * 3) - 4",
    # "sin(3.14) + log(16, 2)",
  ]:
    echo "expr: ", expr
    let ast = parse expr
    assert isvalid ast 
    echo expr, " = ", eval ast
    echo "============================="

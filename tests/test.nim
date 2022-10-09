import std/unittest
import emath


when isMainModule:
  for expr in [
    "1 + 3", 
    "-1 * -3", 
    "1 ^ 8 + -3 * 7 < 1"
  ]:
    let ast = parse expr
    echo "valid? ", isvalid ast 
    echo expr, " = ", eval ast
    echo "============================="

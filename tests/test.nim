import std/unittest
import emath


when isMainModule:
  for t in [
    "1 + 3", 
    "-1 * -3", 
    "1 ^ 8 + -3 * 7 < 1"
  ]:
    echo t, " = ", eval parse t
    echo "============================="

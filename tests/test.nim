import std/unittest
import emath


when isMainModule:
  # let r = parse "1 + 2 * 3 ^ 4 - 5 * 6 * 7"

  for t in ["--1 * 3", "-1 + 3", "1 ^ 8 + -3 * 7 < 1"]:
    echo t, " = ", eval parse t
    echo "============================="
